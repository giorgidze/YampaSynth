module Main where

import MidiSynth
import qualified Codec.SoundFont as SF
import qualified Codec.Midi as Midi
import Player.Wav
import SynthParams

import FRP.Yampa

import Numeric
import System.IO
import System.Environment
import System.Console.GetOpt

data Flag =
    SampleRate String
  | SoundFontFile FilePath
  | MidiFile FilePath
  | Output FilePath
  deriving Show

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["samplerate"] (ReqArg SampleRate "SAMPLERATE")
   "synthesizer sampling rate"
 , Option ['s'] ["soundfont"]  (ReqArg SoundFontFile "FILE")
   "SoundFont FILE"
 , Option ['m'] ["midi"]       (ReqArg MidiFile "FILE")
   "midi FILE to synthesize"
 , Option ['o'] ["output"]     (ReqArg Output "FILE")
   "output FILE (.wav file), if FILE argument equals stdout then the output will be sent to standard output. The output can be piped to an audio/video player (e.g., mplayer)"
 ]

main :: IO ()
main = do
  argv <- getArgs
  (sampleRate', soundFontFile, midiFile, wavFile) <-
    case getOpt Permute options argv of
      ([SampleRate srs, SoundFontFile sf, MidiFile mf, Output wf], _,_) -> do
        sr <- case (readDec srs) of
          [] -> fail "SAMPLERATE must be a decimal integer number"
          (i,_) : _ -> return i
        return (sr, sf, mf, wf)
      _ -> fail $ usageInfo "all options are mandatory\n" options

  hPutStrLn stderr "Importing MIDI File ..."
  eMidi <- Midi.importFile midiFile
  midi <- case  eMidi of
    Left err -> fail err
    Right midi -> return midi
  hPutStrLn stderr "Done."
  
  hPutStrLn stderr "Importing SoundFontFile ... This may take a while ..."
  eSoundFont <- SF.importFile soundFontFile
  soundFont <- case  eSoundFont of
    Left err -> fail err
    Right sf -> return sf
  hPutStrLn stderr "Done."
  
  hPutStrLn stderr $ "Writing WAV audio data to '" ++ wavFile  ++ "' ..."
  let paramsGen = soundFontToSynthParams soundFont
      sf = midiToEventSource midi >>> midiSynth paramsGen
  Player.Wav.play wavFile sampleRate' sf
  hPutStrLn stderr "Done."