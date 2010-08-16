{-# LANGUAGE Arrows #-}

module MidiSynth (
    midiSynth
  , midiToEventSource
  ) where

import qualified SynthParams as SP
import SynthBasics

import Data.Audio
import Codec.Midi hiding (Time)

import FRP.Yampa

import qualified Data.Map as Map

type MidiSynth = SF (Event [Message]) (Sample, Event ())
type Col a = Map.Map (Channel,Key) a

eventToList :: Event [a] -> [a]
eventToList me = if isEvent me then fromEvent me else []

synthParamsToMonoSynth :: SP.SynthParams -> MidiSynth
synthParamsToMonoSynth params = proc me -> do
  e <- iEdge False -< any msgPred (eventToList me)
  s <- oscSampleBased params -< (0,e)  
  (cv,end) <- envGen 0 adsr (Just 2) -< e
  sample <- amp 0.125 (SP.velocity params) -< (s,cv)
--  cv <- oscSine 1.5 >>> arr( + 1) -< 0
--  s <- moogVCF 48000 (19000) 0.0 -< (s,0)  
  returnA -< (sample,end)
  where
  adsr = [(0.001,1.0),(0.001,0.80),(0.24,0.0)]
  msgPred msg = (isNoteOff msg || isNoteOn msg && velocity msg == 0)
             && (key msg == SP.key params)
             && (channel msg == SP.channel params)

midiToEventSource :: Midi -> SF a (Event [Message])
midiToEventSource m = afterEachCat
  $! toRealTime (timeDiv m')
  $! head
  $! tracks m'
  where
  m' = toSingleTrack m

data SynthState = SynthState {
    channelState :: Map.Map Channel (Bank,Preset)
--  , synthParams  :: Map.Map (Bank, Preset, Key, Channel, Velocity)
--                             SP.SynthParams
  }

defaultSynthState :: SynthState
defaultSynthState = SynthState {
    channelState = Map.singleton 9 (128,0)
--  , synthParams = Map.empty
  }

getChannelState :: Channel -> SynthState -> (Bank,Preset)
getChannelState c s = Map.findWithDefault (0,0) c (channelState s)

updateSynthState :: Message -> SynthState -> SynthState
updateSynthState (ProgramChange c p) s = s {channelState =
  Map.insertWith (\_ (b,_) -> (b,p)) c (0,p) (channelState s)}
updateSynthState (ControlChange c 0 b) s = s {channelState = -- Bank Select
  Map.insertWith (\_ (_,p) -> (b,p)) c (b,0) (channelState s)}
updateSynthState _ s = s

triggerChange :: SynthState ->
  SF (Event [Message], Col (Sample, Event ()))
     (Event ([(Channel, Key, Velocity)], [(Channel,Key)], SynthState))
triggerChange initState = proc (me, ses) -> do
  let msgList = eventToList me
      onMsgs = filter (\msg -> isNoteOn msg && velocity msg > 0) msgList
      ons = map (\m -> (channel m, key m, velocity m))  onMsgs
      newState = foldr updateSynthState initState msgList
      offs = Map.keys $ Map.filter (isEvent . snd) ses
  e <- iEdge False -< not (null ons)
                   || not (null offs)
                   || (channelState newState /= channelState initState)
  returnA -< tag e (ons,offs,newState)

performChange ::
  SP.SynthParamsGen
  -> Col MidiSynth
  -> ([(Channel, Key, Velocity)], [(Channel,Key)], SynthState)
  -> SF (Event [Message]) (Col (Sample, Event ()))
performChange paramsGen sfs (ons,offs,synthState) =
  pSwitchB sfs2 (noEvent --> triggerChange synthState) (performChange paramsGen)
  where
  sfs1 = foldr (Map.delete) sfs offs
  sfs2 = foldr (\(c,k,v) col -> insertSynth c k v col) sfs1 ons
  cmbSynths :: MidiSynth -> MidiSynth -> MidiSynth
  cmbSynths newSF oldSF = noEvent >-- proc midiEvent -> do
    s1 <- switch sf (\_ -> constant 0) -< ()
    (s2,e2) <- newSF -< midiEvent
    returnA -< (s1 + s2, e2)
    where
    sf = proc () -> do
      (cv,e) <- envGen 1.0 [(0.001, 0.0)] Nothing -< noEvent
      (s,_) <- oldSF -< noEvent
      returnA -< (s * cv,e)
  
  insertSynth :: Channel -> Key -> Velocity  -> Col MidiSynth -> Col MidiSynth
  insertSynth c k v col = case paramsGen b p c k v of
    (Just params) -> Map.insertWith (cmbSynths) (c,k)
                                    (synthParamsToMonoSynth params) col
    Nothing -> col
    where (b,p) = getChannelState c synthState

midiSynth :: SP.SynthParamsGen -> MidiSynth
midiSynth paramsGen = proc me -> do
  ses <- pSwitchB Map.empty (triggerChange defaultSynthState)
                            (performChange paramsGen) -< me
  end <- iEdge False -< any isTrackEnd (eventToList me)
--  end <- after 15.0 () -< ()
--  smpl <- mixer -< Map.elems $ Map.map fst ses
  let smpl = Map.fold (\(s,_) s' -> s + s') 0 ses
  returnA -< (smpl, end)