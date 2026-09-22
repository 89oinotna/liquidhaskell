{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Liquid.GHC.Plugin.Serialisation (
      -- * Serialising and deserialising things from/to specs.
        serialiseLiquidLib
      , deserialiseLiquidLib

      ) where

import qualified Data.Array                               as Array

import           Control.Monad
import           Control.Concurrent.MVar

import qualified Data.Binary                             as B
import qualified Data.Binary.Builder                     as Builder
import qualified Data.Binary.Put                         as B
import qualified Data.ByteString.Lazy                    as B
import qualified Data.ByteString                         as BS
import           Data.Data (Data)
import           Control.Exception
import           Control.Exception.Backtrace
import           Control.Exception.Context
import           Data.Generics (ext0, gmapAccumT)
import qualified Data.HashMap.Strict                     as M
import           Data.Maybe                               ( listToMaybe, mapMaybe )
import           Data.IORef
import           Data.Unique
import           Data.Word (Word8)
import           GHC.Stack (HasCallStack)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.Weak (Weak, deRefWeak)

import qualified Liquid.GHC.API as GHC
import           Language.Haskell.Liquid.GHC.Plugin.Types (LiquidLib, SpecReference(..), libDeps)
import qualified Language.Haskell.Liquid.GHC.Plugin.Compact as Compact
import qualified Language.Haskell.Liquid.GHC.Plugin.Cache as Cache
import           Language.Haskell.Liquid.Types.Names
import           Language.Haskell.Liquid.UX.Config (Config, specCacheLimit)


--
-- Serialising and deserialising Specs
--

-- Retain the old annotation's type identity solely to diagnose stale interfaces.
newtype LiquidLibBytes = LiquidLibBytes [Word8]

serialiseLiquidLib :: GHC.HscEnv -> LiquidLib -> GHC.TcGblEnv -> IO GHC.Annotation
serialiseLiquidLib env lib tcg = do
    bytes <- B.toStrict <$> encodeLiquidLib lib
    fingerprint <- Compact.payloadId bytes
    Compact.stagePayload tcg bytes
    ifaces <- forM (libDeps lib) $ \ref ->
      GHC.lookupIfaceByModuleHsc env (GHC.unStableModule $ specModule ref) >>=
        maybe (ioError $ userError "LiquidHaskell: dependency interface disappeared during verification") pure
    Compact.stageDependencies tcg ifaces
    pure $ GHC.Annotation (GHC.ModuleTarget $ GHC.tcg_mod tcg) $
      GHC.toSerialized Compact.markerBytes (Compact.payloadMarker fingerprint $ BS.length bytes)

-- GHC's interface cache holds encoded data; this cache holds canonical decoded
-- module specs, never merged transitive closures. Entry and encoded-size limits
-- are opt-in. The EPS weak key releases the entire cache when its
-- compilation session dies, including sessions abandoned by IDE clients.
type LibraryCache = Cache.Cache SpecReference LiquidLib
data SessionCache = SessionCache !Unique !(Weak (IORef GHC.ExternalPackageState)) !LibraryCache

{-# NOINLINE sessionCaches #-}
sessionCaches :: MVar [SessionCache]
sessionCaches = unsafePerformIO $ newMVar []

getLibraryCache :: GHC.HscEnv -> IO LibraryCache
getLibraryCache env = modifyMVar sessionCaches $ \sessions -> do
    found <- findSession sessions
    case found of
      Just cache -> pure (sessions, cache)
      Nothing -> do
        cache <- Cache.newCache
        key <- newUnique
        weak <- mkWeakIORef epsRef $ modifyMVar_ sessionCaches $ \allSessions -> do
          let live = filter (\(SessionCache k _ _) -> k /= key) allSessions
          -- Force the list spine so cleanup cannot leave a filter thunk
          -- retaining the dead session's cache until a future compilation.
          length live `seq` pure live
        pure (SessionCache key weak cache : sessions, cache)
  where
    epsRef = GHC.euc_eps $ GHC.ue_eps $ GHC.hsc_unit_env env
    findSession [] = pure Nothing
    findSession (SessionCache _ weak cache : rest) = do
      alive <- deRefWeak weak
      if alive == Just epsRef then pure (Just cache) else findSession rest

deserialiseLiquidLib :: Config -> GHC.HscEnv -> GHC.Module -> IO (Maybe (SpecReference, LiquidLib))
deserialiseLiquidLib cfg env thisModule = do
    eps <- readIORef $ GHC.euc_eps $ GHC.ue_eps $ GHC.hsc_unit_env env
    home <- GHC.lookupHugByModule thisModule (GHC.hsc_HUG env)
    let homeAnnotations = case home of
          Just info | GHC.mi_module (GHC.hm_iface info) == thisModule ->
            GHC.ifAnnotatedValue <$> GHC.mi_anns (GHC.hm_iface info)
          _ -> []
        annotations decoder =
          mapMaybe (GHC.fromSerialized decoder) homeAnnotations ++
          GHC.findAnns decoder (GHC.eps_ann_env eps) (GHC.ModuleTarget thisModule)
    case listToMaybe $ annotations Compact.PayloadMarker of
      Nothing -> do
        iface <- GHC.lookupIfaceByModuleHsc env thisModule
        -- A compact field without a recognized marker can come from another
        -- plugin build whose annotation TypeRep has a different package ID.
        if not (null $ annotations LiquidLibBytes) || maybe False Compact.hasPayload iface
          then ioError $ userError $ "LiquidHaskell: legacy or incompatible interface for " ++
            GHC.renderModule thisModule ++ ". Rebuild this dependency with the current LiquidHaskell plugin."
          else pure Nothing
      Just marker -> do
        (fingerprint, size) <- either (ioError . userError) pure $ Compact.decodeMarker marker
        let reference = SpecReference (GHC.toStableModule thisModule) fingerprint
        cache <- getLibraryCache env
        lib <- Cache.cached cache limits reference size $ do
          iface <- GHC.lookupIfaceByModuleHsc env thisModule
          bytes <- maybe (pure Nothing) Compact.readPayload iface >>= maybe missingPayload pure
          actual <- Compact.payloadId bytes
          unless (BS.length bytes == size && actual == fingerprint) $
            ioError $ userError $ "LiquidHaskell: corrupt specification for " ++ GHC.renderModule thisModule
          -- Lazy name decoding must retain only the NameCache, not a selector
          -- thunk keeping the entire HscEnv (and our weak session key) alive.
          let nameCache = GHC.hsc_NC env
          nameCache `seq` decodeLiquidLib nameCache (B.fromStrict bytes)
        pure $ Just (reference, lib)
  where
    limits
      | specCacheLimit cfg = Cache.Bounded 128 (64 * 1024 * 1024)
      | otherwise = Cache.Unbounded
    missingPayload = ioError $ userError $ "LiquidHaskell: missing compact specification for " ++
      GHC.renderModule thisModule ++ ". Rebuild this dependency with the current LiquidHaskell plugin."

encodeLiquidLib :: LiquidLib -> IO B.ByteString
encodeLiquidLib lib0 = rethrowWithCallStackIO $ do
    let (lib1, ns) = collectLHNames lib0
    bh <- GHC.openBinMem (1024*1024)
    GHC.putWithUserData GHC.QuietBinIFace GHC.SafeExtraCompression bh ns
    GHC.withBinBuffer bh $ \bs ->
      return $ Builder.toLazyByteString $ B.execPut (B.put lib1) <> Builder.fromByteString bs

decodeLiquidLib :: GHC.NameCache -> B.ByteString -> IO LiquidLib
decodeLiquidLib nameCache bs0 = rethrowWithCallStackIO $ do
    case B.decodeOrFail bs0 of
      Left (_, _, err) -> error $ "decodeLiquidLib: decodeOrFail: " ++ err
      Right (bs1, _, lib) -> do
        bh <- GHC.unsafeUnpackBinBuffer $ B.toStrict bs1
        ns <- GHC.getWithUserData nameCache bh
        let n = fromIntegral $ length ns
            arr = Array.listArray (0, n - 1) ns
        return $ mapLHNames (resolveLHNameIndex arr) lib
  where
    resolveLHNameIndex :: Array.Array Word LHResolvedName -> LHName -> LHName
    resolveLHNameIndex arr lhname =
      case getLHNameResolved lhname of
        LHRIndex i ->
          if i <= snd (Array.bounds arr) then
            makeResolvedLHName (arr Array.! i) (getLHNameSymbol lhname)
          else
            error $ "decodeLiquidLib: index out of bounds: " ++ show (i, Array.bounds arr)
        _ ->
          lhname

newtype AccF a b = AccF { unAccF :: a -> b -> (a, b) }

collectLHNames :: Data a => a -> (a, [LHResolvedName])
collectLHNames t =
    let ((_, _, xs), t') = go (0, M.empty, []) t
     in (t', reverse xs)
  where
    go
      :: Data a
      => (Word, M.HashMap LHResolvedName Word, [LHResolvedName])
      -> a
      -> ((Word, M.HashMap LHResolvedName Word, [LHResolvedName]), a)
    go = gmapAccumT $ unAccF $ AccF go `ext0` AccF collectName

    collectName acc@(sz, m, xs) n = case M.lookup n m of
      Just i -> (acc, LHRIndex i)
      Nothing -> ((sz + 1, M.insert n sz m, n : xs), LHRIndex sz)

-- | Rethrow an exception so we have an indication of where it was thrown in
-- the stack trace.
rethrowWithCallStackIO :: HasCallStack => IO a -> IO a
rethrowWithCallStackIO action = catchNoPropagate action $ \(ExceptionWithContext ctx (e :: SomeException)) -> do
    btAnn <- collectBacktraces
    rethrowIO $ ExceptionWithContext (addExceptionAnnotation btAnn ctx) e
