{-# LANGUAGE Arrows #-}

module SynthBasics (
    oscSine
  , oscSine'
  , amp
  , envGen
  , oscSampleBased
  , mixer
  , moogVCF
  , CV
  , Frequency
  , Resonance
  ) where

import FRP.Yampa
import Data.Audio hiding (sampleRate)
import Codec.Midi hiding (Time)
import SynthParams

type CV = Double
type Frequency = Double
type Resonance =  Double -- 0 - 1

oscSine :: Frequency -> SF CV Sample
oscSine f0 = proc cv -> do
  let f = f0 * (2 ** cv)
  phi <- integral -< 2 * pi * f
  returnA -< sin phi

oscSine' :: Frequency -> SF a Sample
oscSine' f0 = time >>> arr (sin . (2 * pi * f0 *))

amp :: CV -> Velocity -> SF (Sample, CV) Sample
amp volumeLevel vel = proc (smpl,cv) -> do
  returnA -< (fromIntegral vel / 127)  * smpl * cv * volumeLevel

envGen :: CV -> [(Time,CV)] -> Maybe Int
            -> SF (Event ()) (CV, Event ())
envGen l0 tls Nothing  = envGenAux l0 tls &&& after (sum (map fst tls)) ()
envGen l0 tls (Just n) =
  switch (proc release -> do
            l <- envGenAux l0 tls1 -< ()
            returnA -< ((l, noEvent), release `tag` l))
         (\l -> envGenAux l tls2 &&& after (sum (map fst tls2)) ())
  where
  (tls1, tls2) = splitAt n tls

envGenAux :: CV -> [(Time,CV)] -> SF a CV
envGenAux l0 tls = afterEach trs >>> hold r0 >>> integral >>^ (+ l0)
  where
  (r0, trs) = toRates l0 tls

toRates :: CV -> [(Time, CV)] -> (Double, [(Time, Double)])
toRates _  []          = (0, [])
toRates l0 ((t,l):tls) = (((l - l0) / t), trAux t l tls)

trAux :: Time -> CV -> [(Time, CV)] -> [(Time, Double)] 
trAux t _ []            = [(t, 0)]
trAux t l ((t',l'):tls) = (t, (l' - l) / t') : trAux t' l' tls
  
moogVCF :: Int -> Frequency -> Resonance -> SF (Sample, CV) Sample
moogVCF sr f0 r = proc (x, cv) -> do
  let f = f0 * (2 ** cv)
      g = 1 - exp (-2 * pi * f / fromIntegral sr)
  rec ya <- moogAux -< (x - 4 * r * y, g)
      yb <- moogAux -< (ya, g)
      yc <- moogAux -< (yb, g)
      yd <- moogAux -< (yc, g)
      
      -- 1/2-sample delay for phase compensation
      ye <- iPre 0 -< yd
      y  <- iPre 0 -< (ye + yd) / 2
  returnA -< y
  where
  vt = 40000 -- 2 * Vt
  moogAux = proc (x,g) -> do
    rec let y = ym1 + vt * g * (tanh (x / vt) - tanh (ym1 / vt))
        ym1 <- iPre 0 -< y
    returnA -< y

oscSampleBasedAux :: SynthParams -> SF CV (Sample, Int, Int)
oscSampleBasedAux params = proc cv -> do
  phi <- integral -< (freq params) / (rootFreq params) * (2 ** cv)
  let (n,f) = properFraction (phi * (fromIntegral $ sampleRate params))
      p1 = posAux n
      s1 = g p1
      p2 = posAux (n + 1)
      s2 = g p2
  returnA -< (s1 + f * (s2 - s1), p2, n)
  where
  g = sampleGen params
  s = start params
  m = startLoop params
  e = endLoop params
  posAux n = if (s + n < m)
    then s + n
    else m + mod (s + n - m) (e - m)

oscSampleBased :: SynthParams -> SF (CV, Event ()) Sample
oscSampleBased params | sampleMode params == NoLoop =
  switch (
    proc (cv,_) -> do
      (smpl,_,n) <- oscSampleBasedAux params -< cv
      e <- iEdge False -< start params + n >= end params
      returnA -< (smpl,e)
    ) (\_ -> constant 0.0)
oscSampleBased params | sampleMode params == ContLoop =
  proc (cv,_) -> do
    (smpl,_,_) <- oscSampleBasedAux params -< cv
    returnA -< smpl
oscSampleBased params {- | sampleMode params == Audio.PressLoop -} =
  proc (cv,e) -> do
    smpl <- switch auxSF1 auxSF2 -< (cv,e)
    returnA -< smpl
  where
  auxSF1 = proc (cv,e) -> do
    (smpl,p,_) <- oscSampleBasedAux params -< cv
    returnA -< (smpl, tag e p )
  auxSF2 s = oscSampleBased (params {start = s, sampleMode = NoLoop}) 

mixer :: SF [Sample] Sample
mixer = arr sum