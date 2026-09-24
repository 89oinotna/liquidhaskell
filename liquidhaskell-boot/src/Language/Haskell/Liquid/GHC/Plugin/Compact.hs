{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Large LH payloads live in an extensible interface field. Only their
-- version and fingerprint live in GHC's boxed-byte annotations. Keeping
-- the fingerprint in an annotation makes it participate in GHC's ordinary
-- interface fingerprinting and recompilation checks.
module Language.Haskell.Liquid.GHC.Plugin.Compact
  ( PayloadId
  , PayloadMarker(..)
  , payloadId
  , payloadMarker
  , decodeMarker
  , stagePayload
  , stageDependencies
  , installInterfaceHook
  , readPayload
  , hasPayload
  , writePayload
  ) where

import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Dynamic
import Data.Typeable (typeOf, typeRep, Proxy(..))
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Word
import Foreign.Ptr (castPtr)
import qualified Liquid.GHC.API as GHC


-- | Fingerprint of the encoded specification payload. The pair holds the two
-- 64-bit words of the 128-bit 'GHC.Fingerprint' produced by 'GHC.fingerprintData',
-- in the same order as that constructor's fields.
-- Stored in the interface marker and dependency references to detect payload
-- changes or mismatches. Together with the full module identity, it forms the
-- key used to reuse decoded specifications in the session cache.
type PayloadId = (Word64, Word64)

-- | Small annotation containing the 'B.encode' representation of this tuple:
--
-- @
-- (version, (fingerprintWord1, fingerprintWord2))
--   :: (Word32, (Word64, Word64))
-- @
--
-- * @version@ is the marker format version, currently 1.
-- * The two fingerprint words form the 'PayloadId' computed from the complete
--   encoded specification payload, in 'GHC.Fingerprint' constructor order.
--
-- 'B.encode' writes these unsigned words consecutively in big-endian order:
--
-- @
-- Byte offsets   Contents
--  0..3          Format version (Word32)
--  4..11         First fingerprint word (Word64)
-- 12..19         Second fingerprint word (Word64)
-- @
--
-- 'markerBytes' holds these 20 raw bytes as a list for 'GHC.toSerialized',
-- obtained by unpacking the encoded tuple. The list is not itself encoded
-- with 'B.encode', so it contributes no list-length prefix. The 20-byte count
-- excludes GHC's annotation wrapper and the list's in-memory overhead.
-- The specification payload itself lives in the extensible interface field.
-- Keeping the fingerprint in an annotation makes specification changes
-- participate in GHC's interface fingerprinting and recompilation checks.
newtype PayloadMarker = PayloadMarker { markerBytes :: [Word8] }

newtype PendingPayload = PendingPayload BS.ByteString
newtype PendingUsages = PendingUsages [GHC.Usage]

fieldName :: GHC.FieldName
fieldName = "liquidhaskell.spec.v1"

payloadId :: BS.ByteString -> IO PayloadId
payloadId bytes = BS.useAsCStringLen bytes $ \(ptr, size) -> do
  GHC.Fingerprint a b <- GHC.fingerprintData (castPtr ptr) size
  pure (a, b)

payloadMarker :: PayloadId -> PayloadMarker
payloadMarker fingerprint =
  PayloadMarker $ BL.unpack $ B.encode (1 :: Word32, fingerprint)

decodeMarker :: PayloadMarker -> Either String PayloadId
decodeMarker (PayloadMarker bytes) = case B.decodeOrFail (BL.pack bytes) of
  Left (_, _, err) -> Left $ "Malformed LiquidHaskell interface marker: " ++ err
  Right (rest, _, (version :: Word32, fingerprint))
    | version /= 1 -> Left "Unsupported LiquidHaskell interface version; rebuild dependencies."
    | not (BL.null rest) -> Left "Malformed LiquidHaskell interface marker."
    | otherwise -> Right fingerprint

-- The module's existing typed TH-state map gives this payload the same lifetime
-- as its GHC.TcGblEnv. A private TypeRep key cannot collide with user TH state, and
-- avoids a global pending-payload table retaining abandoned/failed compilations.
stagePayload :: GHC.TcGblEnv -> BS.ByteString -> IO ()
stagePayload tcg bytes = atomicModifyIORef' (GHC.tcg_th_state tcg) $ \state ->
  (M.insert (typeOf (PendingPayload bytes)) (toDyn $ PendingPayload bytes) state, ())

-- GHC's entity-level home-module usages can overlook changes to module
-- annotations. LH consumes the whole specification, so record whole-module ABI
-- usages for home modules as well as package modules. GHC's checker resolves
-- these by full module identity in either interface table.
stageDependencies :: GHC.TcGblEnv -> [GHC.ModIface] -> IO ()
stageDependencies tcg ifaces = do
  usages <- mapM usage ifaces
  atomicModifyIORef' (GHC.tcg_th_state tcg) $ \state ->
    (M.insert (typeOf (PendingUsages usages)) (toDyn $ PendingUsages usages) state, ())
  where
    usage iface =
      let mdl = GHC.mi_module iface
          fingerprint = GHC.mi_mod_hash iface
      in mdl `seq` fingerprint `seq` pure (GHC.UsagePackageModule mdl fingerprint False)

addUsages :: [GHC.Usage] -> GHC.ModIface_ phase -> GHC.ModIface_ phase
addUsages usages iface = GHC.set_mi_self_recomp
  ((\info -> info { GHC.mi_sr_usages = usages ++ GHC.mi_sr_usages info }) <$> GHC.mi_self_recomp_info iface) iface

-- Simple interfaces omit annotations in GHC 9.14. Restore our marker and run
-- normal fingerprinting so specification changes affect the module ABI in
-- -fno-code mode too. The original declaration bodies and metadata are reused.
rebuildSimpleIface :: GHC.HscEnv -> BS.ByteString -> GHC.ModIface -> IO GHC.ModIface
rebuildSimpleIface env bytes iface = do
    fingerprint <- payloadId bytes
    let marker = GHC.IfaceAnnotation (GHC.ModuleTarget $ GHC.mi_module iface) $
          GHC.toSerialized markerBytes (payloadMarker fingerprint)
    GHC.mkFullIface env (GHC.set_mi_anns (marker : GHC.mi_anns iface) partial) Nothing Nothing GHC.NoStubs []
  where
    partial =
      GHC.set_mi_decls (map snd $ GHC.mi_decls iface) $
      GHC.set_mi_simplified_core (GHC.mi_simplified_core iface) $
      GHC.set_mi_mod_info (GHC.mi_mod_info iface) $
      GHC.set_mi_deps (GHC.mi_deps iface) $
      GHC.set_mi_exports (GHC.mi_exports iface) $
      GHC.set_mi_fixities (GHC.mi_fixities iface) $
      GHC.set_mi_warns (GHC.mi_warns iface) $
      GHC.set_mi_anns (GHC.mi_anns iface) $
      GHC.set_mi_defaults (GHC.mi_defaults iface) $
      GHC.set_mi_insts (GHC.mi_insts iface) $
      GHC.set_mi_fam_insts (GHC.mi_fam_insts iface) $
      GHC.set_mi_rules (GHC.mi_rules iface) $
      GHC.set_mi_trust (GHC.mi_trust iface) $
      GHC.set_mi_trust_pkg (GHC.mi_trust_pkg iface) $
      GHC.set_mi_complete_matches (GHC.mi_complete_matches iface) $
      GHC.set_mi_docs (GHC.mi_docs iface) $
      GHC.set_mi_top_env (GHC.mi_top_env iface) $
      GHC.set_mi_ext_fields (GHC.mi_ext_fields iface) $
      GHC.set_mi_self_recomp (GHC.mi_self_recomp_info iface) $
      GHC.emptyPartialModIface (GHC.mi_module iface)

writePayload :: BS.ByteString -> GHC.ModIface_ phase -> IO (GHC.ModIface_ phase)
writePayload bytes iface = do
  fields <- GHC.writeField fieldName bytes (GHC.mi_ext_fields iface)
  pure $ GHC.set_mi_ext_fields fields iface

readPayload :: GHC.ModIface -> IO (Maybe BS.ByteString)
readPayload = GHC.readField fieldName . GHC.mi_ext_fields

hasPayload :: GHC.ModIface -> Bool
hasPayload = M.member fieldName . GHC.getExtensibleFields . GHC.mi_ext_fields

installInterfaceHook :: GHC.HscEnv -> GHC.HscEnv
installInterfaceHook env = env { GHC.hsc_hooks = hooks { GHC.runPhaseHook = Just $ GHC.PhaseHook run } }
  where
    hooks = GHC.hsc_hooks env
    previous :: GHC.TPhase a -> IO a
    previous = case GHC.runPhaseHook hooks of
      Nothing -> GHC.runPhase
      Just (GHC.PhaseHook hook) -> hook

    run :: GHC.TPhase a -> IO a
    run phase@(GHC.T_HscPostTc hscEnv summary (GHC.FrontendTypecheck tcg) _ _) = do
      state <- readIORef (GHC.tcg_th_state tcg)
      let pending = M.lookup (typeRep (Proxy :: Proxy PendingPayload)) state >>= fromDynamic
          usages = case M.lookup (typeRep (Proxy :: Proxy PendingUsages)) state >>= fromDynamic of
            Just (PendingUsages xs) -> xs
            Nothing -> []
      result <- previous phase
      case pending of
        Nothing -> pure result
        Just (PendingPayload bytes) -> case result of
          recomp@GHC.HscRecomp { GHC.hscs_partial_iface = iface } -> do
            iface' <- writePayload bytes $ addUsages usages iface
            pure recomp { GHC.hscs_partial_iface = iface' }
          GHC.HscUpdate iface -> do
            rebuilt <- rebuildSimpleIface hscEnv bytes $ addUsages usages iface
            iface' <- writePayload bytes rebuilt
            -- GHC writes simple (-fno-code/boot) interfaces inside PostTc.
            -- Rewrite with the field attached, respecting GHC's write flags
            -- and dynamic-too handling.
            GHC.hscMaybeWriteIface (GHC.hsc_logger hscEnv) (GHC.hsc_dflags hscEnv)
              True iface' Nothing (GHC.ms_location summary)
            pure $ GHC.HscUpdate iface'
    run phase = previous phase
