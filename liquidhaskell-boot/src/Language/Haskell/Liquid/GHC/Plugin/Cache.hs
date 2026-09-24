{-# LANGUAGE BangPatterns #-}

-- | A cache with optional limits. The lock covers a cache miss and insertion, so
-- concurrent readers reuse an entry while it remains retained. Loading an
-- entry must not recursively access this cache.
module Language.Haskell.Liquid.GHC.Plugin.Cache
  ( Cache, Limits(..), newCache, cached ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Data.List (sortOn)

-- | Independent entry-count and encoded-byte limits. Nothing is unlimited;
-- zero for either limit disables retention, without preventing loading.
data Limits = Limits !(Maybe Int) !(Maybe Int)

-- The cache is a mutable map from keys to entries, with an access counter for
-- least-recently-used eviction. The cache is not persistent across GHC sessions.
--
-- The 'MVar' protects @(accessCounter, entries)@, including loading a missing
-- value and inserting it, so concurrent requests can reuse the loaded value.
--
-- * The outer 'Integer' is a logical access counter, incremented on each
--   successful lookup (hit or miss). 
-- * The 'M.Map' associates each key @k@ with
--   @(lastAccess, encodedBytes, value)@. 
--
-- * The key is a module identity and
--   payload fingerprint, distinguishing specification versions.
-- * The entry's 'Integer' records the access counter at its most recent lookup.
-- * The entry's 'Int' is the encoded payload length in bytes, supplied when the
--   value is loaded.
-- * The entry's @v@ is the cached value itself (a decoded specification library
--   in the plugin).
newtype Cache k v = Cache (MVar (Integer , M.Map k (Integer, Int, v)))

newCache :: IO (Cache k v)
newCache = Cache <$> newMVar (0, M.empty)

-- | The cached function returns a retained value for a key, 
-- or runs the supplied loader on a miss.
--
-- Side Effects:
-- * The cache lock covers lookup, loading, and updating the state. Concurrent
--   requests reuse the loaded value while it remains retained.
-- * Each successful call advances the access counter and marks a retained entry
--   as most recently used. 
-- * Least-recently-used entries are evicted until the bounds are met.
-- * If loading or updating throws, the exception propagates and the
--   previous cache state is restored.
--
-- Preconditions:
--
-- * Encoded sizes and any configured limits must be non-negative. This
--   function does not validate those inputs.
-- * A key must consistently identify the same value and encoded size. A hit
--   uses the stored value and size, ignoring the supplied loader and size;
--   changed specifications therefore require a different key.
-- * The loader must not call 'cached' on this same cache, or wait for another
--   operation that needs its lock. The lock is held while the loader runs,
--   so such a dependency would deadlock.
cached
  :: Ord k
  => Cache k v
  -> Limits
  -- ^ Independent entry-count and total encoded-byte bounds for this lookup.
  -- 'Nothing' leaves a bound unlimited; zero for either disables retention.
  -> k
  -- ^ Identity of the requested value. 
  -> Int
  -- ^ Encoded payload length in bytes, recorded on a miss for byte-budget
  -- accounting. 
  -> IO v
  -- ^ Action that loads the value on a miss, while holding the cache lock.
  -> IO v
  -- ^ The retained or newly loaded value. Loader and cache-update exceptions propagate.
cached (Cache state) limits key weight load =
  modifyMVar state $ \(clock, entries) -> do
    let !next = clock + 1
    case M.lookup key entries of
      Just (_, oldWeight, value) ->
        let !entries' = retain next oldWeight value entries
        in pure ((next, entries'), value)
      Nothing -> do
        !value <- load
        let !entries' = retain next weight value entries
        -- Evaluate eviction before publishing the state. A deferred trim could
        -- otherwise keep evicted or oversized values alive until another hit.
        pure ((next, entries'), value)
  where
    retain next entryWeight value entries = case limits of
      Limits Nothing Nothing -> M.insert key (next, entryWeight, value) entries
      Limits maxEntries maxBytes -> trim maxEntries maxBytes $
        if not (within entryWeight maxBytes) || disabled maxEntries || disabled maxBytes
          then M.delete key entries
          else M.insert key (next, entryWeight, value) entries

    trim maxEntries maxBytes entries
      | disabled maxEntries || disabled maxBytes = M.empty
      | otherwise =
        -- Multiple valid Int weights can overflow an Int total. Accumulate in
        -- Integer so even a user-supplied maxBound budget is enforced correctly.
        evict (M.size entries) (sum [toInteger w | (_, w, _) <- M.elems entries]) entries oldestFirst
      where
        -- Sort only if eviction is needed. Switching from an unbounded cache
        -- must not repeatedly scan all entries for each value we remove.
        oldestFirst = sortOn (\(_, (age, _, _)) -> age) $ M.toList entries
        evict count bytes remaining oldest
          | within count maxEntries && within bytes (toInteger <$> maxBytes) = remaining
          | otherwise = case oldest of
              [] -> remaining
              (oldKey, (_, oldWeight, _)) : rest ->
                evict (count - 1) (bytes - toInteger oldWeight) (M.delete oldKey remaining) rest

    disabled = maybe False (<= 0)

within :: Ord a => a -> Maybe a -> Bool
within value = maybe True (value <=)
