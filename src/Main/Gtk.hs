module Main where

import MidiSynth
import qualified Codec.SoundFont as SF
import Player.Gtk
import SynthParams

import Numeric
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
 ]

main :: IO ()
main = do
  argv <- getArgs
  (sampleRate', soundFontFile) <-
    case getOpt Permute options argv of
      ([SampleRate srs, SoundFontFile sf], _,_) -> do
        sr <- case (readDec srs) of
          [] -> fail "SAMPLERATE must be a decimal integer number"
          (i,_) : _ -> return i
        return (sr, sf)
      _ -> fail $ usageInfo "all options are mandatory\n" options

  putStrLn "Importing SoundFont file ... This may take a while ..."
  eSoundFont <- SF.importFile soundFontFile
  soundFont <- case  eSoundFont of
    Left err -> fail err
    Right sf -> return sf
  putStrLn "Done."
  
  putStrLn "If you are getting interruptions decrase sampling rate."
  let paramsGen = soundFontToSynthParams soundFont
  Player.Gtk.play sampleRate' 512 2 (midiSynth paramsGen)
  putStrLn "Done."