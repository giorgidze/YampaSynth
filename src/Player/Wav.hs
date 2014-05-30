module Player.Wav (play) where

import Data.Audio
import Codec.Wav
import Codec.ByteString.Builder

import FRP.Yampa

import Data.Int
import Data.Array.IArray
import Data.ByteString.Lazy
import System.IO

play :: FilePath -> Int -> SF () (Sample, Event ()) -> IO ()
play filePath sampleRate' signalFunction = do
  h <- case filePath of
         "stdout" -> return stdout
         _        -> openFile filePath WriteMode
  hSetBuffering h (BlockBuffering (Just 1024))
  let a = Audio {
        sampleRate = sampleRate'
      , channelNumber = 1
      , sampleData = array (0,0) [(0,0)]
      } :: Audio Int16
  hPut h (toLazyByteString $ buildWav a)
  let sense _ = return (1.0 / fromIntegral (sampleRate a), Just ())
      actuate _ (_,e) | isEvent e = return True
      actuate _ (s,_) = do
        hPut h (toLazyByteString $ buildSample ((fromSample s) :: Int16))
        return False
  reactimate (return ()) sense actuate signalFunction
  hClose h
  correctWavHeader filePath
  
correctWavHeader :: FilePath -> IO ()
correctWavHeader filePath = do
  h <- openFile filePath ReadWriteMode
  hSetBuffering h NoBuffering
  s <- hFileSize h
  hSeek h AbsoluteSeek 0x04
  hPut h (toLazyByteString $ putWord32le $ fromIntegral $ s - 0x04 - 4)
  hSeek h AbsoluteSeek 0x28
  hPut h (toLazyByteString $ putWord32le $ fromIntegral $ s - 0x28 - 4)
  hClose h
