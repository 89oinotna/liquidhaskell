{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (isNothing)
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath
import System.IO
import System.Mem (performGC)
import System.Mem.Weak (deRefWeak)

import qualified Liquid.GHC.API as GHC
import qualified GHC.Paths as Paths

import qualified Language.Haskell.Liquid.GHC.Plugin.Cache as Cache
import qualified Language.Haskell.Liquid.GHC.Plugin.Compact as Compact
import qualified "liquidhaskell-boot" Language.Haskell.Liquid.UX.CmdLine as CmdLine
import "liquidhaskell-boot" Language.Haskell.Liquid.UX.Config (specCacheLimit)

main :: IO ()
main = do
  testCache
  testUnboundedCache
  testCacheOption
  testMarker
  forM_ ["-fno-code", "-fobject-code", "-fbyte-code"] $ \mode ->
    testInterfaces Paths.libdir mode
  putStrLn "Plugin storage tests passed."

check :: String -> Bool -> IO ()
check label success = unless success $ fail label

testCache :: IO ()
testCache = do
  cache <- Cache.newCache
  calls <- newIORef (0 :: Int)
  let fetch key weight = Cache.cached cache (Cache.Bounded 2 100) key weight $
        atomicModifyIORef' calls $ \n -> (n + 1, n + 1)
  a <- fetch ("A", 1 :: Int) 4
  a' <- fetch ("A", 1) 4
  check "cache hit must reuse decoded value" (a == a')
  b <- fetch ("B", 1) 4
  _ <- fetch ("A", 1) 4
  _ <- fetch ("C", 1) 4
  b' <- fetch ("B", 1) 4
  check "least recently used entry must be evicted" (b /= b')
  aNew <- fetch ("A", 2) 4
  aOld <- fetch ("A", 1) 4
  check "different fingerprints must not share decoded specs" (aNew /= aOld)
  big <- fetch ("large", 1) 101
  big' <- fetch ("large", 1) 101
  check "oversized entries must not remain cached" (big /= big')
  weighted <- Cache.newCache
  _ <- Cache.cached weighted (Cache.Bounded 10 5) (1 :: Int) 3 (pure (1 :: Int))
  _ <- Cache.cached weighted (Cache.Bounded 10 5) 2 3 (pure 2)
  evicted <- Cache.cached weighted (Cache.Bounded 10 5) 1 3 (pure 3)
  check "encoded-size limit must evict entries" (evicted == 3)

  shared <- Cache.newCache
  started <- newEmptyMVar
  finish <- newEmptyMVar
  results <- replicateM 8 newEmptyMVar
  count <- newIORef (0 :: Int)
  forM_ results $ \result -> void $ forkIO $ do
    value <- try $ Cache.cached shared (Cache.Bounded 10 100) () 1 $ do
      modifyIORef' count (+ 1)
      putMVar started ()
      readMVar finish
      pure (42 :: Int)
    putMVar result (value :: Either SomeException Int)
  takeMVar started
  putMVar finish ()
  values <- mapM takeMVar results
  check "concurrent requests must all complete" (all (either (const False) (== 42)) values)
  readIORef count >>= check "concurrent misses must decode once" . (== 1)
  failed <- try $ Cache.cached shared (Cache.Bounded 10 100) () 1 (pure 0)
  check "a cached value remains usable" (either (const False) (== 42) (failed :: Either SomeException Int))
  retry <- Cache.newCache
  (_ :: Either SomeException Int) <- try $ Cache.cached retry (Cache.Bounded 1 10) () 1 (throwIO $ userError "decode failed")
  Cache.cached retry (Cache.Bounded 1 10) () 1 (pure (7 :: Int)) >>= check "failed decoding must not poison the cache" . (== 7)

  checkReleased "oversized values must be collectible without another lookup" 11 False
  checkReleased "evicted values must be collectible without another lookup" 1 True
  where
    checkReleased label weight evict = do
      cache <- Cache.newCache
      weak <- do
        value <- newIORef ()
        weak <- mkWeakIORef value (pure ())
        void $ Cache.cached cache (Cache.Bounded 1 10) (0 :: Int) weight (pure value)
        pure weak
      when evict $ void $ Cache.cached cache (Cache.Bounded 1 10) 1 1 (newIORef ())
      performGC
      released <- isNothing <$> deRefWeak weak
      -- Keep the cache alive across GC, without touching it until after the
      -- weak-pointer check. Deferred eviction would keep the old value alive.
      void $ Cache.cached cache (Cache.Bounded 1 10) 2 1 (newIORef ())
      check label released

testUnboundedCache :: IO ()
testUnboundedCache = do
  cache <- Cache.newCache
  let keys = [1 .. 256 :: Int]
      weight = 64 * 1024 * 1024 + 1
  forM_ keys $ \key -> void $ Cache.cached cache Cache.Unbounded key weight (pure key)
  forM_ keys $ \key -> do
    value <- Cache.cached cache Cache.Unbounded key weight (fail "unbounded cache evicted an entry")
    check "unbounded mode reuses entries beyond both optional limits" (value == key)
  -- A hit must also enforce a new policy; otherwise a module enabling limits
  -- could keep the preceding module's unbounded cache alive indefinitely.
  value <- Cache.cached cache (Cache.Bounded 128 (64 * 1024 * 1024)) 256 weight
    (fail "switching policies should reuse a cached value before dropping it")
  check "oversized hit remains usable when limits are restored" (value == 256)
  forM_ keys $ \key -> do
    reloaded <- Cache.cached cache Cache.Unbounded key weight (pure (-key))
    check "restoring size limits evicts oversized entries" (reloaded == -key)

  counted <- Cache.newCache
  forM_ [1 .. 4 :: Int] $ \key -> void $ Cache.cached counted Cache.Unbounded key 1 (pure key)
  _ <- Cache.cached counted (Cache.Bounded 2 100) 1 1 (fail "expected a cache hit")
  Cache.cached counted Cache.Unbounded 4 1 (pure 0) >>=
    check "restoring count limit preserves the other newest entry" . (== 4)
  Cache.cached counted Cache.Unbounded 2 1 (pure 0) >>=
    check "restoring count limit evicts the oldest entry" . (== 0)

testCacheOption :: IO ()
testCacheOption = bracket (lookupEnv "LIQUIDHASKELL_OPTS") restore $ \_ -> do
  unsetEnv "LIQUIDHASKELL_OPTS"
  let options = CmdLine.getOpts . ("--smtsolver=z3mem" :)
  check "cache limits are disabled by default" (not $ specCacheLimit CmdLine.defConfig)
  options [] >>= check "parsing without cache flags keeps limits disabled" . not . specCacheLimit
  options ["--spec-cache-limit"] >>= check "the option enables cache limits" . specCacheLimit
  options ["--spec-cache-limit", "--no-spec-cache-limit"] >>=
    check "explicit opt-out overrides opt-in" . not . specCacheLimit
  options ["--no-spec-cache-limit", "--spec-cache-limit"] >>=
    check "the last cache option wins" . specCacheLimit
  setEnv "LIQUIDHASKELL_OPTS" "--spec-cache-limit"
  options [] >>= check "environment can enable cache limits" . specCacheLimit
  options ["--no-spec-cache-limit"] >>=
    check "plugin options override environment cache limits" . not . specCacheLimit
  where
    restore = maybe (unsetEnv "LIQUIDHASKELL_OPTS") (setEnv "LIQUIDHASKELL_OPTS")

testMarker :: IO ()
testMarker = do
  let bytes = BS.replicate (4 * 1024 * 1024) 42
  fingerprint <- Compact.payloadId bytes
  let marker = Compact.payloadMarker fingerprint (BS.length bytes)
  check "annotation must stay small" (length (Compact.markerBytes marker) <= 32)
  check "marker round trip" (Compact.decodeMarker marker == Right (fingerprint, BS.length bytes))
  check "truncated marker must be rejected" $ either (const True) (const False) $
    Compact.decodeMarker (Compact.PayloadMarker [0])
  check "unknown marker version must be rejected" $ either (const True) (const False) $
    Compact.decodeMarker (Compact.PayloadMarker $ [0, 0, 0, 2] ++ drop 4 (Compact.markerBytes marker))
  check "trailing marker bytes must be rejected" $ either (const True) (const False) $
    Compact.decodeMarker (Compact.PayloadMarker $ Compact.markerBytes marker ++ [0])
  iface <- Compact.writePayload bytes $ GHC.emptyFullModIface $
    GHC.mkModule GHC.mainUnit (GHC.mkModuleName "Storage")
  Compact.readPayload iface >>= check "compact payload round trip" . (== Just bytes)

testInterfaces :: FilePath -> String -> IO ()
testInterfaces libdir mode = withTestDirectory $ \dir -> do
  let a = dir </> "StorageA.hs"
      b = dir </> "StorageB.hs"
      source suffix = "module StorageA where\na :: Int\na = 1\n" ++ suffix
  writeFile a (source "")
  writeFile b "module StorageB where\nimport StorageA\nb :: Int\nb = a\n"
  revision <- newIORef (1 :: Int)
  checks <- newIORef (0 :: Int)
  let payload = do
        rev <- readIORef revision
        pure $ BS.replicate (256 * 1024) (fromIntegral rev)
      plugin = GHC.defaultPlugin
        { GHC.driverPlugin = \_ -> pure . Compact.installInterfaceHook
        , GHC.pluginRecompile = GHC.purePlugin
        , GHC.typeCheckResultAction = \_ _ tcg -> do
            env <- GHC.getTopEnv
            bytes <- liftIO payload
            when (GHC.moduleNameString (GHC.moduleName $ GHC.tcg_mod tcg) == "StorageB") $ liftIO $ do
              let dep = GHC.mkModule (GHC.moduleUnit $ GHC.tcg_mod tcg) (GHC.mkModuleName "StorageA")
              iface <- GHC.lookupIfaceByModuleHsc env dep >>= maybe (fail "missing imported interface") pure
              Compact.readPayload iface >>= check (mode ++ ": imported compact payload") . (== Just bytes)
              Compact.stageDependencies tcg [iface]
              modifyIORef' checks (+ 1)
            fingerprint <- liftIO $ Compact.payloadId bytes
            liftIO $ Compact.stagePayload tcg bytes
            let ann = GHC.Annotation (GHC.ModuleTarget $ GHC.tcg_mod tcg) $
                  GHC.toSerialized Compact.markerBytes (Compact.payloadMarker fingerprint $ BS.length bytes)
            pure tcg { GHC.tcg_anns = ann : GHC.tcg_anns tcg }
        }
      compile = GHC.runGhc (Just libdir) $ do
        flags <- GHC.getSessionDynFlags
        logger <- GHC.getLogger
        (flags', _, _) <- GHC.parseDynamicFlags logger flags $ map GHC.noLoc
          [mode, "-fwrite-interface", "-i" ++ dir, "-odir", dir, "-hidir", dir]
        _ <- GHC.setSessionDynFlags flags'
        env <- GHC.getSession
        GHC.setSession $ env { GHC.hsc_plugins = (GHC.hsc_plugins env)
          { GHC.staticPlugins = [GHC.StaticPlugin (GHC.PluginWithArgs plugin []) False] } }
        target <- GHC.guessTarget b Nothing Nothing
        GHC.setTargets [target]
        result <- GHC.load GHC.LoadAllTargets
        liftIO $ case result of
          GHC.Succeeded -> pure ()
          GHC.Failed -> fail $ mode ++ ": compilation failed"
  compile
  readIORef checks >>= check (mode ++ ": initial import checked") . (== 1)
  -- Force only the importer to be rechecked in a fresh session. The dependency
  -- must be read back from its .hi, not from a pending-payload cache.
  appendFile b "\n-- Reload importer\n"
  compile
  readIORef checks >>= check (mode ++ ": interface survived disk reload") . (== 2)
  writeIORef revision 2
  writeFile a (source "-- Change only the LH payload and source hash\n")
  compile
  readIORef checks >>= check (mode ++ ": changed spec must recompile importer") . (== 3)
  compile
  readIORef checks >>= check (mode ++ ": unchanged specs must not force recompilation") . (== 3)

withTestDirectory :: (FilePath -> IO a) -> IO a
withTestDirectory = bracket create removePathForcibly
  where
    create = do
      tmp <- getTemporaryDirectory
      (path, tempHandle) <- openTempFile tmp "lh-plugin-storage"
      hClose tempHandle
      removeFile path
      createDirectory path
      pure path
