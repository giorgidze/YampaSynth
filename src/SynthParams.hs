module SynthParams (
   SynthParams (..)
 , soundFontToSynthParams
 , SynthParamsGen
 , toFreq
 ) where

import qualified Codec.SoundFont as SF
import qualified Data.Audio as Audio
import qualified Codec.Midi as Midi

import Data.List
import Data.Array.IArray
import Data.Word

import Control.Monad
import Control.Applicative


toFreq :: Midi.Key -> Int -> Double
toFreq key' correction' =
  let k = fromIntegral key' + (fromIntegral correction' / 100) - 69
  in 440 * 2 ** (k / 12)

data SynthParams = SynthParams {
    sampleGen :: Int -> Audio.Sample
  , start :: !Int
  , end :: !Int
  , startLoop :: !Int
  , endLoop :: !Int
  , sampleMode :: !Audio.SampleMode
  , sampleRate :: !Int
--  , envelope :: [(Double,Double)] -- [(Time, CV)]
  , rootKey :: !Int
  , rootFreq :: !Double
  , key :: !Int
  , freq :: !Double
  , channel :: !Int
  , velocity :: !Int
  } -- deriving (Show)


type SynthParamsGen =
     Midi.Bank -> Midi.Preset -> Midi.Channel -> Midi.Key -> Midi.Velocity
  -> Maybe SynthParams


data SoundFontParams = SoundFontParams {
    sfStart :: Word
  , sfEnd :: Word
  , sfStartLoop :: Word
  , sfEndLoop :: Word
  , sfSampleMode :: Audio.SampleMode
  , sfSampleRate :: Word
  , sfRootKey :: Word
  , sfRootKeyCorrection :: Int
  , sfKey :: Word
  , sfVel :: Word
  , sfCoarseTune :: Int
  , sfFineTune :: Int
--  , sfDelayVolEnv :: Int
--  , sfAttackVolEnv :: Int
--  , sfHoldVolEnv :: Int
--  , sfDecayVolEnv :: Int
--  , sfSustainVolEnv :: Int
--  , sfReleaseVolEnv :: Int
  }


-- For subset of generators I am dealing here this definition is OK
-- But in general preset and instrument generator should be handeled separatly
applyGen :: SF.Generator -> SoundFontParams -> SoundFontParams
applyGen g p = case g of
  SF.StartAddressOffset v ->
    p {sfStart = fromIntegral $ (fromIntegral $ sfStart p) + v}
  SF.EndAddressOffset v -> 
    p {sfEnd = fromIntegral $ (fromIntegral $ sfEnd p) + v}
  SF.LoopStartAddressOffset v ->
    p {sfStartLoop = fromIntegral $ (fromIntegral $ sfStartLoop p) + v}
  SF.LoopEndAddressOffset v ->
    p {sfEndLoop = fromIntegral $ (fromIntegral $ sfEndLoop p) + v}
  SF.StartAddressCoarseOffset v -> 
    p {sfStart = fromIntegral $ (fromIntegral $ sfStart p) + v * 2 ^ (15::Int)}
  SF.EndAddressCoarseOffset v -> 
    p {sfEnd = fromIntegral $ (fromIntegral $ sfEnd p) + v * 2 ^ (15::Int)}
  SF.LoopStartAddressCoarseOffset v -> 
    p {sfStartLoop = fromIntegral $ (fromIntegral $ sfStartLoop p) + v * 2 ^ (15::Int)}
  SF.LoopEndAddressCoarseOffset v -> 
    p {sfEndLoop = fromIntegral $ (fromIntegral $ sfEndLoop p) + v * 2 ^ (15::Int)}

  SF.RootKey v -> p {sfRootKey = v}
  SF.SampleMode v -> p {sfSampleMode = v}

  SF.Key v -> p {sfKey = v}
  SF.Vel v -> p {sfVel = v}
  
  SF.CoarseTune v -> p {sfCoarseTune = sfCoarseTune p + v}
  SF.FineTune v -> p {sfFineTune = sfFineTune p + v}
  
--  SF.DelayVolEnv v -> p {sfDelayVolEnv = v}
--  SF.AttackVolEnv v -> p {sfAttackVolEnv = v}
--  SF.HoldVolEnv v -> p {sfHoldVolEnv = v}
--  SF.DecayVolEnv v -> p {sfDecayVolEnv = v}
--  SF.SustainVolEnv v -> p {sfSustainVolEnv = v}
--  SF.ReleaseVolEnv v -> p {sfReleaseVolEnv = v}

  _ -> p

-- Is can be implemented way more efficiently by translating soundFont
-- to intermediate data structure more suitable for real-time scenarios
-- But this is ok for now, it implements subset of SoundFont standard
soundFontToSynthParams :: SF.SoundFont -> SynthParamsGen
soundFontToSynthParams sf b p c k v = do
  (phdrNdx,phdr) <- find (\(_,phdr) -> SF.bank phdr == fromIntegral b
                              && SF.preset phdr == fromIntegral p)
                   (assocs phdrs)
  let pBagNdx1 = SF.presetBagNdx phdr
      pBagNdx2 = SF.presetBagNdx (phdrs ! (phdrNdx + 1)) - 1
      pgenss'' = [ [ pgens ! i' | i' <- [pGenNdx1 .. pGenNdx2]]
                    |   i <- [pBagNdx1 .. pBagNdx2]
                      , let pGenNdx1 = (SF.genNdx  $ pbags ! i)
                      , let pGenNdx2 = ((SF.genNdx $ pbags ! (i + 1)) - 1)]
  guard $ (not $ null pgenss'')
  let (pGlobalGens,pgenss') = case head pgenss'' of
         [] -> ([],pgenss'')
         gs -> if (not $ SF.isInstIndex $ last gs)
                 then (gs, tail pgenss'')
                 else ([], pgenss'')
  (igens',iGlobalGens,pgens') <- foldr (<|>) Nothing (map f pgenss')
  
  SF.SampleIndex smplNdx <- find SF.isSampleIndex igens'
  let shdr = shdrs ! smplNdx    
      defSFParams = SoundFontParams {
          sfStart = SF.start shdr
        , sfEnd = SF.end shdr
        , sfStartLoop = SF.startLoop shdr
        , sfEndLoop = SF.endLoop shdr
        , sfRootKey = SF.originalPitch shdr
        , sfRootKeyCorrection = SF.pitchCorrection shdr
        , sfSampleRate = SF.sampleRate shdr
        , sfSampleMode = Audio.NoLoop
        , sfKey = fromIntegral k
        , sfVel = fromIntegral v
        , sfCoarseTune = 0
        , sfFineTune =  0

--        , sfDelayVolEnv = -12000
--        , sfAttackVolEnv = -12000
--        , sfHoldVolEnv = -12000
--        , sfDecayVolEnv = -12000
--        , sfSustainVolEnv = 0
--        , sfReleaseVolEnv = -12000
        }
      sfParams = foldr applyGen defSFParams (pGlobalGens ++ pgens' ++ iGlobalGens ++ igens')
  let synthParams =  SynthParams {
    sampleGen = \i -> Audio.toSample $ (SF.smpl $ SF.sdta sf) ! i
  , start = fromIntegral $ sfStart sfParams
  , end = fromIntegral $ sfEnd sfParams
  , startLoop = fromIntegral $ sfStartLoop sfParams
  , endLoop = fromIntegral $ sfEndLoop sfParams
  , sampleMode = sfSampleMode sfParams
  , sampleRate = fromIntegral $ sfSampleRate sfParams
  , rootKey = fromIntegral $ sfRootKey sfParams
  , rootFreq = toFreq (fromIntegral $ sfRootKey sfParams)
                      (sfRootKeyCorrection sfParams)
--  , envelope = [
--        ( 2 ** ((fromIntegral $ sfDelayVolEnv   sfParams)  / 1200) , 0)
--      , ( 2 ** ((fromIntegral $ sfAttackVolEnv  sfParams)  / 1200) , 1)
--      , ( 2 ** ((fromIntegral $ sfHoldVolEnv    sfParams)  / 1200) , 1)
--      , ( 2 ** ((fromIntegral $ sfDecayVolEnv   sfParams)  / 1200) , 
--          let l = sfSustainVolEnv sfParams
--          in if (l <= 0) then 1 else 1 / ((fromIntegral l) * 10 ** (1 / 200))
--        )
--      , ( 2 ** ((fromIntegral $ sfReleaseVolEnv sfParams)  / 1200) , 0)
--    ]
  , key  = k
  , freq = toFreq ((fromIntegral $ sfKey sfParams) + sfCoarseTune sfParams)
                  (sfFineTune sfParams)
  , channel = c
  , velocity = fromIntegral $ sfVel sfParams
  }
  return 
--    $  trace (show pGlobalGens ++ "\n" ++ show pgens' ++ "\n" 
--               ++ show iGlobalGens ++ "\n" ++ show igens' ++ "\n"
--               ++ show (envelope synthParams)
--             )

    $! synthParams
  where
  pdta = SF.pdta sf
  phdrs = SF.phdrs pdta
  pbags = SF.pbags pdta
  pgens = SF.pgens pdta
  insts = SF.insts pdta
  ibags = SF.ibags pdta
  igens = SF.igens pdta
  shdrs = SF.shdrs pdta
  
  areSuitableGens gens = case gens of
    (SF.KeyRange k1 k2 : _)     | fromIntegral k < k1 || k2 < fromIntegral k -> False
    (_ : SF.VelRange v1 v2 : _) | fromIntegral v < v1 || v2 < fromIntegral v -> False
    (SF.VelRange v1 v2 : _)     | fromIntegral v < v1 || v2 < fromIntegral v -> False
    _ -> True  
  
  f pgens'= do
    SF.InstIndex instNdx <- find (SF.isInstIndex) pgens'
    let iBagNdx1 = SF.instBagNdx $ insts ! instNdx
        iBagNdx2 = (SF.instBagNdx $ insts ! (instNdx + 1)) - 1
        igenss'' = [ [ igens ! i' | i' <- [iGenNdx1 .. iGenNdx2]]
                    |   i <- [iBagNdx1 .. iBagNdx2]
                      , let iGenNdx1 = (SF.genNdx  $ ibags ! i)
                      , let iGenNdx2 = ((SF.genNdx $ ibags ! (i + 1)) - 1)]
        (iGlobalGens,igenss') = case head igenss'' of
          [] -> ([],igenss'')
          gs -> if (not $ SF.isSampleIndex $ last gs)
                  then (gs, tail igenss'')
                  else ([], igenss'')                  
    igens' <- find areSuitableGens igenss'
    return $! (igens', iGlobalGens, pgens')