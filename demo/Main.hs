{-# LANGUAGE Arrows #-}

module Main where

import qualified SynthBasics as Synth
import qualified Data.Audio as Audio
import Player.Wav (play)
import FRP.Yampa

sciFi :: SF () Audio.Sample
sciFi = proc () -> do
  und   <- arr (*0.2) <<< Synth.oscSine 3.0  -< 0
  swp   <- arr (+1.0) <<< integral     -< -0.25
  audio <- Synth.oscSine 440           -< und + swp
  returnA -< audio

envBell :: SF (Event ()) (Synth.CV, Event ())
envBell = Synth.envGen 0 [(0.05,1),(1.5,0)] Nothing

bell :: Synth.Frequency -> SF () (Audio.Sample, Event ())
bell f = proc () -> do
  m            <- Synth.oscSine (2.33 * f)  -< 0
  audio        <- Synth.oscSine f           -< 2.0 * m 
  (ampl, end)  <- envBell           -< noEvent
  returnA -< (audio * ampl, end)

scale :: SF () (Audio.Sample, Event ())
scale =  (  afterEach  [  (0.0, 60), (2.0, 62), (2.0, 64),
                          (2.0, 65), (2.0, 67), (2.0, 69),
                          (2.0, 71), (2.0, 72)]
            >>>  constant ()
                 &&& arr (fmap (\k -> (bell $ toFreq k) >>> arr fst))
            >>> rSwitch (constant 0))
         &&& after 16 () 

toFreq :: Int -> Double
toFreq n = 440.0 * (2.0 ** (((fromIntegral n) - 69.0) / 12.0))

main :: IO ()
main = do
  let sampleRate = 44100
  play "oscSine.wav" sampleRate ((constant 0 >>> Synth.oscSine 440 >>> arr ( * 0.9)) &&& after 4 ())
  play "vibrato.wav" sampleRate ((constant 0 >>> Synth.oscSine 5.0 >>> arr (* 0.05) >>> Synth.oscSine 440 >>> arr ( * 0.9)) &&& after 4 ()) 
  play "sciFi.wav" sampleRate ((sciFi >>> arr (* 0.9)) &&& after 5 ())
  play "bell.wav" sampleRate (bell 440 >>> arr (\ (s,e) -> (0.9 * s,e)))
  play "scale.wav" sampleRate (scale >>> arr(\ (s,e) -> (0.9 * s,e)))
  return()