-- | A bounded cache. The lock covers a cache miss as well as insertion, so
-- concurrent readers cannot decode the same interface more than once. Loading
-- an entry must not recursively access this cache.
module Language.Haskell.Liquid.GHC.Plugin.Cache
  ( Cache, newCache, cached ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Data.List (minimumBy)
import Data.Ord (comparing)

data Cache k v = Cache !Int !Int !(MVar (Integer, M.Map k (Integer, Int, v)))

newCache :: Int -> Int -> IO (Cache k v)
newCache entries bytes = Cache entries bytes <$> newMVar (0, M.empty)

-- | The weight is the encoded payload size, not an estimate of live heap.
-- Oversized entries are usable but are not retained by the cache.
cached :: Ord k => Cache k v -> k -> Int -> IO v -> IO v
cached (Cache maxEntries maxBytes state) key weight load =
  modifyMVar state $ \(clock, entries) -> do
    let next = clock + 1
    next `seq` case M.lookup key entries of
      Just (_, oldWeight, value) ->
        let entries' = M.insert key (next, oldWeight, value) entries
        in entries' `seq` pure ((next, entries'), value)
      Nothing -> do
        value <- load
        let entries'
              | weight > maxBytes || maxEntries <= 0 = entries
              | otherwise = trim $ M.insert key (next, weight, value) entries
        -- Evaluate eviction before publishing the state. A deferred trim could
        -- otherwise keep evicted or oversized values alive until another hit.
        value `seq` entries' `seq` pure ((next, entries'), value)
  where
    trim entries
      | M.size entries <= maxEntries && sum [w | (_, w, _) <- M.elems entries] <= maxBytes = entries
      | M.null entries = entries
      | otherwise = trim $ M.delete oldest entries
      where
        oldest = fst $ minimumBy (comparing (\(_, (age, _, _)) -> age)) $ M.toList entries
