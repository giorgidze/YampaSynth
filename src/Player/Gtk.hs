module Player.Gtk (Player.Gtk.play) where

import Paths_YampaSynth (getDataFileName)

import Player.OpenAL

import FRP.Yampa

import Data.Audio
import Codec.Midi

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent
import Control.Applicative

import Graphics.UI.Gtk hiding (eventKeyName)
import Graphics.UI.Gtk.Gdk.Events hiding (Event)
import Graphics.UI.Gtk.Glade

play :: Int -> Int -> Int -> SF (Event [Message]) (Sample, Event ()) -> IO ()
play sampleRate' sampleNumber' numBuffs synth = do
  (device,context,pSource,pBuffers) <- initOpenAL numBuffs
  mVarMessage <- newEmptyMVar
  mVarReplyGui <- newEmptyMVar
  mVarKeyboardActive <- newEmptyMVar
  refKeyboardStatus <- newIORef $ (Map.empty :: Map.Map String ())

  _ <- initGUI

  guiDescFile <- getDataFileName "gui/YampaSynth.glade"
  Just xml <- xmlNew guiDescFile

  window   <- xmlGetWidget xml castToWindow "window1"
  _ <- onDestroy window $ do
    deInitOpenAL device context pSource pBuffers
    mainQuit
  let
    onKeyPressWindow e = do
      b <- isEmptyMVar mVarKeyboardActive
      if b
        then return ()
        else do
          let k = eventKeyName e
          m <- readIORef refKeyboardStatus
          if Map.member k m
            then return ()
            else do
              writeIORef refKeyboardStatus (Map.insert k () m)
              putMVar mVarMessage (NoteOn  {channel = 0, key = str2key k, velocity = 85})
      return True
    onKeyReleaseWindow e = do
      b <- isEmptyMVar mVarKeyboardActive
      if b
        then return ()
        else do
          let k = eventKeyName e
          modifyIORef refKeyboardStatus (\m -> Map.delete k m)
          putMVar mVarMessage (NoteOff  {channel = 0, key = str2key k, velocity = 85})
      return True
  _ <- onKeyPress window onKeyPressWindow
  _ <- onKeyRelease window onKeyReleaseWindow

  combobox <- xmlGetWidget xml castToComboBox "combobox1"
  comboBoxSetActive combobox 0

  _ <- on combobox changed $ do
    b <- isEmptyMVar mVarKeyboardActive
    if b
      then return ()
      else do
        i <- comboBoxGetActive combobox
        if i < 0
          then return ()
          else putMVar mVarMessage (ProgramChange { channel = 0,  preset = fromIntegral i })
        return ()

  playButton <- xmlGetWidget xml castToButton "button1"
  stopButton <- xmlGetWidget xml castToButton "button2"
  widgetSetSensitivity stopButton False

  _ <- onClicked playButton $ do
    _ <- forkIO $ do frpSynth sampleRate' pSource pBuffers sampleNumber' synth noEvent $ 
                         maybe noEvent (Event . (:[])) <$> tryTakeMVar mVarMessage
                     putMVar mVarReplyGui ()

    widgetSetSensitivity playButton False
    widgetSetSensitivity stopButton True
    i <- comboBoxGetActive combobox
    if i < 0
      then return ()
      else putMVar mVarMessage (ProgramChange { channel = 0,  preset = fromIntegral i })
    putMVar mVarKeyboardActive ()

  _ <- onClicked stopButton $ do
    putMVar mVarMessage TrackEnd
    takeMVar mVarReplyGui
    widgetSetSensitivity stopButton False
    widgetSetSensitivity playButton True
    takeMVar mVarKeyboardActive

  widgetShowAll window
  mainGUI

str2key :: String -> Key
str2key s = case s of
  "p" -> 64
  "0" -> 63
  "o" -> 62
  "9" -> 61
  "i" -> 60
  "u" -> 59
  "7" -> 58
  "y" -> 57
  "6" -> 56
  "t" -> 55
  "5" -> 54
  "r" -> 53
  "e" -> 52
  "3" -> 51
  "w" -> 50
  "2" -> 49
  "q" -> 48

  "m" -> 59 - 12
  "j" -> 58 - 12
  "n" -> 57 - 12
  "h" -> 56 - 12
  "b" -> 55 - 12
  "g" -> 54 - 12
  "v" -> 53 - 12
  "c" -> 52 - 12
  "d" -> 51 - 12
  "x" -> 50 - 12
  "s" -> 49 - 12
  "z" -> 48 - 12

  _   -> 0