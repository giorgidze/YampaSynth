module Main where

import MidiSynth
import qualified Codec.SoundFont as SF
import qualified Codec.Midi as Midi
import Player.OpenAL
import SynthParams

import FRP.Yampa

import Numeric
import System.IO
import System.Environment
import System.Console.GetOpt

import Control.Monad
import Control.Applicative

data Flag =
    SampleRate String
  | SoundFontFile FilePath
  | MidiFile FilePath
  deriving Show

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["samplerate"] (ReqArg SampleRate "SAMPLERATE")
   "synthesizer sampling rate"
 , Option ['s'] ["soundfont"]  (ReqArg SoundFontFile "FILE")
   "SoundFont FILE"
 , Option ['m'] ["midi"]       (ReqArg MidiFile "FILE")
   "midi FILE to synthesize"
 ]

main :: IO ()
main = do
  argv <- getArgs
  let (opts,nopts,errs) = getOpt Permute options argv
  printErrs "not valid options" nopts
  mapM_ putStrLn errs
  when (not $ null errs) . ioError . userError $ concat errs ++ usageInfo "aborting" options
  go . concat $ [getSampleRate, getSoundFontFile, getMidiFile] <*> opts

printErrs :: String -> [String] -> IO ()
printErrs s x = when (not $ null x) . putStrLn $ s ++ ": " ++ show x

-- the record transformer approach described here is probably a better way to achieve option order insensitivity
-- http://haskell.org/ghc/docs/6.12.1/html/libraries/base-4.2.0.0/System-Console-GetOpt.html#4

getSampleRate :: MonadPlus m => Flag -> m String
getSampleRate (SampleRate x) = return x
getSampleRate _              = mzero

getSoundFontFile :: MonadPlus m => Flag -> m FilePath
getSoundFontFile (SoundFontFile x) = return x
getSoundFontFile _                 = mzero

getMidiFile :: MonadPlus m => Flag -> m FilePath
getMidiFile (MidiFile x) = return x
getMidiFile _            = mzero

go :: [String] -> IO ()
go [srs, soundFontFile, midiFile] = do
  sampleRate' <- case (readDec srs) of
    []        -> fail "SAMPLERATE must be a positive decimal integer number"
    (i,_) : _ -> (putStrLn $ show i) >> return i

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
  
  hPutStrLn stderr "Now you should hear something ..."
  hPutStrLn stderr "If you are getting interruptions decrase sampling rate."
  let paramsGen = soundFontToSynthParams soundFont
      signalFunction = midiToEventSource midi >>> midiSynth paramsGen
  Player.OpenAL.play sampleRate' (1 * 1024) 2 signalFunction
  putStrLn "Done."

go _ = fail $ usageInfo "all options are mandatory\n" options
