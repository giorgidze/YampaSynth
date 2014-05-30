{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Player.OpenAL (
   Player.OpenAL.play
 , initOpenAL
 , deInitOpenAL
 , frpSynth
 , Chunk (..)
 ) where

import Foreign (Storable, Ptr, Bits, isSigned, mallocArray, free, pokeElemOff, peekElemOff, sizeOf)

import FRP.Yampa
import Sound.OpenAL
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Audio
import Data.IORef
import Data.Int
import Data.Maybe


play :: Int -> Int -> Int -> SF () (Sample, Event ()) -> IO ()
play sampleRate' sampleNumber' numBuffs sf = do
  (device,context,pSource,pBuffers) <- initOpenAL numBuffs
  frpSynth sampleRate' pSource pBuffers sampleNumber' sf () (return ())
  deInitOpenAL device context pSource pBuffers

frpSynth :: Int -> Source -> [Buffer] -> Int -> SF a (Sample, Event b) -> a -> IO a -> IO ()
frpSynth sampleRate' pSource pBuffers sampleNumber' sf ret senseEvt = do
  mVarMaybeChunk <- newEmptyMVar
  mVarReplyPlayer <- newEmptyMVar

  _ <- forkIO $ process sampleRate' pSource pBuffers [] mVarMaybeChunk mVarReplyPlayer

  ir <- newIORef (0 :: Int)

  chunkData' <- mallocArray sampleNumber'
  let sense = (\x -> (1.0 / fromIntegral sampleRate', x)) . Just <$> senseEvt -- ghc 6.12 required for TupleSections :(
      chunk = Chunk chunkData' sampleNumber'
      actuate _ (s,e) = if (isEvent e)
        then return True
        else do
          i <- readIORef ir
          let samp = fromSample s :: Int16 -- the only place we have to specify our sample representation
          when (i /= 0 || samp /= 0) $ do -- don't put leading zeros in a chunk
                  pokeElemOff chunkData' i samp
                  if i == (sampleNumber' - 1)
                     then do
                          putMVar mVarMaybeChunk $ Just chunk
                          takeMVar mVarReplyPlayer
                          writeIORef ir 0
                     else writeIORef ir (i + 1)
          return False
  
  reactimate (return ret) (const sense) actuate sf

  i <- readIORef ir
  putMVar mVarMaybeChunk . Just $ chunk {numElems = i}
  takeMVar mVarReplyPlayer
  putMVar mVarMaybeChunk Nothing
  takeMVar mVarReplyPlayer
  free chunkData'

initOpenAL :: Int -> IO (Device, Context, Source, [Buffer])
initOpenAL numBuffs = do
  mDevice <- openDevice Nothing
  case mDevice of
    Nothing -> fail "opening OpenAL device"
    Just device -> do
      mContext <- createContext device []
      case mContext of
        Nothing -> fail "opening OpenAL context"
        Just context -> do
          currentContext $= Just context
          [pSource] <- genObjectNames 1
          pBuffers <- genObjectNames numBuffs
          printErrs
          return (device,context,pSource,pBuffers)
        
deInitOpenAL :: Device -> Context -> Source -> [Buffer] -> IO ()
deInitOpenAL device context pSource pBuffers = do
  dequeue pSource
  deleteObjectNames [pSource]
  deleteObjectNames pBuffers
  currentContext $= Nothing
  destroyContext context
  whenM (not <$> closeDevice device) $ fail "closing OpenAL device"
  printErrs
    
data Chunk a = Chunk { chunkData :: Ptr a
                     , numElems  :: Int
                     } deriving (Eq, Show)

lastInd :: (Storable a) => (a -> Bool) -> Chunk a -> IO (Maybe Int)
lastInd p c = do
  (_,mInd) <- untilM (\(i,x) -> isJust x || i < 0)
                     (\(i,_) -> do e <- peekElemOff (chunkData c) i
                                   return (i-1, if p e then Just i else Nothing)
                                   )
                     (numElems c - 1,Nothing)
  return $ (+ 1) <$> mInd

process :: (Storable a, Bits a) => Int -> Source -> [Buffer] -> [Buffer] -> MVar (Maybe (Chunk a)) -> MVar () -> IO ()
process sampleRate' pSource freeBuffers usedBuffers mVarMaybeChunk mVarReply = do
  mChunk <- takeMVar mVarMaybeChunk
  void $ reply mChunk (\chunk -> do
    mInd <- lastInd (/= 0) chunk -- we aren't sent chunks with leading zeros
    (f,u) <- reply mInd (\ind -> do
      (buff,newFree,newUsed) <- if null freeBuffers
         then do waitForBuffer pSource
                 [b] <- unqueueBuffers pSource (1 :: ALsizei)
                 return (b,[],tail usedBuffers ++ [b])
         else do let h = head freeBuffers
                 return (h, tail freeBuffers, usedBuffers ++ [h])
      ((bufferData buff) $=) =<< createBufferData sampleRate' chunk ind
      _ <- reply Nothing undefined
      queueBuffers pSource [buff]
      whenM ((/= Playing) <$> (get $ sourceState pSource)) $ Sound.OpenAL.play [pSource]
      printErrs
      return (newFree,newUsed)
      )
    process sampleRate' pSource f u mVarMaybeChunk mVarReply
    return (undefined,undefined)
    )
  dequeue pSource
  where reply = flip . maybe $ putMVar mVarReply undefined >> return (freeBuffers,usedBuffers)

printErrs :: IO ()
printErrs = do e <- get alErrors
               when (not $ null e) . putStrLn $ show e

dequeue :: Source -> IO ()
dequeue pSource = waitForSource pSource >> buffer pSource $= Nothing

createBufferData :: (Storable a, Bits a) => Int -> Chunk a -> Int -> IO (BufferData a)
createBufferData sampleRate' chunk n = do
  ex <- peekElemOff (chunkData chunk) 0
  let elemSize = sizeOf ex
      format = case elemSize of
                 2 -> Mono16
                 1 -> Mono8
                 _ -> error "1 or 2 byte buffer required"
  when (not $ isSigned ex) $ fail "signed buffer required" -- how enforce these statically?
  return $ BufferData (MemoryRegion (chunkData chunk) (fromIntegral $ n * elemSize))
                      format
                      (fromIntegral sampleRate')


waitForBuffer :: Source -> IO () -- better to express using untilM_
waitForBuffer s = do b <- (> 0) <$> (get $ buffersProcessed s)
                     if b then return () else threadDelay 10 >> waitForBuffer s

waitForSource :: Source -> IO ()
waitForSource pSource = whenM ((== Playing) <$> (get $ sourceState pSource)) delWait
  where delWait = do threadDelay 10 -- micro seconds
                     waitForSource pSource

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x | p x       = return x
             | otherwise = f x >>= untilM p f

whenM :: (Monad m, Functor m) => m Bool -> m () -> m ()
whenM test action = join $ flip when action <$> test
