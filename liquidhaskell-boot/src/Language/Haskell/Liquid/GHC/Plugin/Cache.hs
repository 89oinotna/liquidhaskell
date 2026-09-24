{-# LANGUAGE BangPatterns #-}

-- | A cache retaining every loaded value for reuse. The lock covers a miss and
-- insertion, so concurrent readers reuse the same entry. Loading an
-- entry must not recursively access this cache.
module Language.Haskell.Liquid.GHC.Plugin.Cache
  ( Cache, newCache, cached ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as M

-- | A mutable map from keys @k@ to retained values @v@, protected by an 'MVar'.
-- The plugin uses a module identity and payload fingerprint as the key, and a
-- decoded specification library as the value. Entries remain available for
-- the lifetime of the cache; the plugin ties that lifetime to its GHC session.
newtype Cache k v = Cache (MVar (M.Map k v))

newCache :: IO (Cache k v)
newCache = Cache <$> newMVar M.empty

-- | The cached function returns a retained value for a key,
-- or runs the supplied loader on a miss.
--
-- Side Effects:
-- * The cache lock covers lookup, loading, and updating the state. Concurrent
--   requests reuse the loaded value. A hit leaves the map unchanged; a miss
--   inserts the newly loaded value for subsequent requests.
-- * A loaded value and the updated map are evaluated to weak head normal form
--   before publishing the state. Values are not deeply evaluated.
-- * If loading or updating throws, the exception propagates and the
--   previous cache state is restored. External effects of the loader are not
--   rolled back.
--
-- Preconditions:
--
-- * A key must consistently identify the same value. A hit uses the stored
--   value without running the supplied loader;
--   changed specifications therefore require a different key.
-- * The loader must not call 'cached' on this same cache, or wait for another
--   operation that needs its lock. The lock is held while the loader runs,
--   so such a dependency would deadlock.
cached
  :: Ord k
  => Cache k v
  -- ^ Shared mutable cache to query and update.
  -> k
  -- ^ Identity of the requested value.
  -> IO v
  -- ^ Action that loads the value on a miss, while holding the cache lock.
  -> IO v
  -- ^ The retained or newly loaded value. Loader and cache-update exceptions propagate.
cached (Cache state) key load =
  modifyMVar state $ \entries ->
    case M.lookup key entries of
      Just value -> pure (entries, value)
      Nothing -> do
        !value <- load
        let !entries' = M.insert key value entries
        pure (entries', value)
