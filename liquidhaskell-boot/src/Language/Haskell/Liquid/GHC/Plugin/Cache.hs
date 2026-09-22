-- | A cache with optional limits. The lock covers a cache miss and insertion, so
-- concurrent readers cannot decode the same interface more than once. Loading
-- an entry must not recursively access this cache.
module Language.Haskell.Liquid.GHC.Plugin.Cache
  ( Cache, Limits(..), newCache, cached ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Data.List (sortOn)

data Limits = Bounded !Int !Int | Unbounded

newtype Cache k v = Cache (MVar (Integer, M.Map k (Integer, Int, v)))

newCache :: IO (Cache k v)
newCache = Cache <$> newMVar (0, M.empty)

-- | The weight is the encoded payload size, not an estimate of live heap.
-- Oversized entries are usable but are not retained by the cache.
-- Limits apply to every lookup, including hits, because modules sharing a
-- session can select different policies through their LiquidHaskell options.
cached :: Ord k => Cache k v -> Limits -> k -> Int -> IO v -> IO v
cached (Cache state) limits key weight load =
  modifyMVar state $ \(clock, entries) -> do
    let next = clock + 1
    next `seq` case M.lookup key entries of
      Just (_, oldWeight, value) ->
        let entries' = retain next oldWeight value entries
        in entries' `seq` pure ((next, entries'), value)
      Nothing -> do
        value <- load
        let entries' = retain next weight value entries
        -- Evaluate eviction before publishing the state. A deferred trim could
        -- otherwise keep evicted or oversized values alive until another hit.
        value `seq` entries' `seq` pure ((next, entries'), value)
  where
    retain next entryWeight value entries = case limits of
      Unbounded -> M.insert key (next, entryWeight, value) entries
      Bounded maxEntries maxBytes -> trim maxEntries maxBytes $
        if entryWeight > maxBytes || maxEntries <= 0
          then M.delete key entries
          else M.insert key (next, entryWeight, value) entries

    trim maxEntries maxBytes entries =
      evict (M.size entries) (sum [w | (_, w, _) <- M.elems entries]) entries oldestFirst
      where
        -- Sort only if eviction is needed. Switching from an unbounded cache
        -- must not repeatedly scan all entries for each value we remove.
        oldestFirst = sortOn (\(_, (age, _, _)) -> age) $ M.toList entries
        evict count bytes remaining oldest
          | count <= maxEntries && bytes <= maxBytes = remaining
          | otherwise = case oldest of
              [] -> remaining
              (oldKey, (_, oldWeight, _)) : rest ->
                evict (count - 1) (bytes - oldWeight) (M.delete oldKey remaining) rest
