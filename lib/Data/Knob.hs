{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Data.Knob
-- Copyright: 2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: GHC only
--
-- Create memory&#8208;backed 'IO.Handle's, referencing virtual files. This is
-- mostly useful for testing 'IO.Handle'&#8208;based APIs without having to
-- interact with the filesystem.
--
-- > import Data.ByteString (pack)
-- > import Data.Knob
-- > import System.IO
-- >
-- > main = do
-- >     knob <- newKnob (pack [])
-- >     h <- newFileHandle knob "test.txt" WriteMode
-- >     hPutStrLn h "Hello world!"
-- >     hClose h
-- >     bytes <- Data.Knob.getContents knob
-- >     putStrLn ("Wrote bytes: " ++ show bytes)
module Data.Knob
	( Knob
	, newKnob
	, Data.Knob.getContents
	, setContents
	
	, newFileHandle
	, withFileHandle
	) where

import qualified Control.Concurrent.MVar as MVar
import           Control.Exception (bracket, throwIO)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Typeable (Typeable)
import qualified Foreign
import qualified GHC.IO.Buffer as IO
import qualified GHC.IO.BufferedIO as IO
import qualified GHC.IO.Device as IO
import qualified GHC.IO.Exception as IO
import qualified GHC.IO.Handle as IO
import qualified System.IO as IO

-- | A knob is a basic virtual file, which contains a byte buffer. A knob can
-- have multiple 'IO.Handle's open to it, each of which behaves like a standard
-- file handle.
--
-- Use 'getContents' and 'setContents' to inspect and modify the knob&#8217;s
-- byte buffer.
newtype Knob = Knob (MVar.MVar ByteString)

data Device = Device IO.IOMode (MVar.MVar ByteString) (MVar.MVar Int)
	deriving (Typeable)

instance IO.IODevice Device where
	ready _ _ _ = return True
	close _ = return ()
	isTerminal _ = return False
	isSeekable _ = return True
	
	seek (Device _ _ var) IO.AbsoluteSeek off = do
		checkOffset off
		MVar.modifyMVar_ var (\_ -> return (fromInteger off))
	
	seek (Device _ _ var) IO.RelativeSeek off = do
		MVar.modifyMVar_ var (\old_off -> do
			let new_off = toInteger old_off + off
			checkOffset new_off
			return (fromInteger new_off))
	
	seek dev@(Device _ _ off_var) IO.SeekFromEnd off = do
		MVar.modifyMVar_ off_var (\_ -> do
			size <- IO.getSize dev
			let new_off = size + off
			checkOffset new_off
			return (fromInteger new_off))
	
	tell (Device _ _ var) = fmap toInteger (MVar.readMVar var)
	getSize (Device _ var _) = do
		bytes <- MVar.readMVar var
		return (toInteger (Data.ByteString.length bytes))
	setSize dev size = setDeviceSize dev size
	devType _ = return IO.RegularFile

checkOffset :: Integer -> IO ()
checkOffset off = when (toInteger (maxBound :: Int) < off) (throwIO err) where
	err = IO.IOError Nothing IO.InvalidArgument "" "offset > (maxBound :: Int)" Nothing Nothing

setDeviceSize :: Device -> Integer -> IO ()
setDeviceSize (Device mode bytes_var _) size = checkSize >> setBytes where
	intSize :: Int
	intSize = fromInteger size
	
	checkSize = when (size > toInteger (maxBound :: Int)) $ do
		throwIO (IO.IOError Nothing IO.InvalidArgument "" "size > (maxBound :: Int)" Nothing Nothing)
	
	setBytes = MVar.modifyMVar_ bytes_var $ \bytes -> case mode of
		IO.ReadMode -> throwIO (IO.IOError Nothing IO.IllegalOperation "" "handle in ReadMode" Nothing Nothing)
		IO.WriteMode -> return (Data.ByteString.replicate intSize 0)
		IO.ReadWriteMode -> return (clip bytes)
		IO.AppendMode -> return (clip bytes)
	
	clip bytes = case intSize - Data.ByteString.length bytes of
		padLen | padLen > 0 -> Data.ByteString.append bytes (Data.ByteString.replicate padLen 0)
		_ -> Data.ByteString.take intSize bytes

instance IO.BufferedIO Device where
	newBuffer _ = IO.newByteBuffer 4096
	
	fillReadBuffer dev buf = do
		(numRead, newBuf) <- IO.fillReadBuffer0 dev buf
		return (maybe 0 id numRead, newBuf)
	
	fillReadBuffer0 (Device _ bytes_var pos_var) buf = do
		MVar.withMVar bytes_var $ \bytes -> do
			MVar.modifyMVar pos_var $ \pos -> do
				if pos >= Data.ByteString.length bytes
					then return (pos, (Nothing, buf))
					else do
						let chunk = Data.ByteString.take (IO.bufSize buf) (Data.ByteString.drop pos bytes)
						unsafeUseAsCStringLen chunk $ \(chunkPtr, chunkLen) -> do
							Foreign.withForeignPtr (IO.bufRaw buf) $ \ptr -> do
								Foreign.copyArray ptr (Foreign.castPtr chunkPtr) chunkLen
							return (pos + chunkLen, (Just chunkLen, (buf { IO.bufL = 0, IO.bufR = chunkLen })))
	
	flushWriteBuffer (Device _ bytes_var pos_var) buf = do
		MVar.modifyMVar_ bytes_var $ \bytes -> do
			MVar.modifyMVar pos_var $ \pos -> do
				let (before, after) = Data.ByteString.splitAt pos bytes
				let padding = Data.ByteString.replicate (pos - Data.ByteString.length before) 0
				
				let bufStart ptr = Foreign.castPtr (Foreign.plusPtr ptr (IO.bufL buf))
				let bufLen = IO.bufR buf - IO.bufL buf
				bufBytes <- Foreign.withForeignPtr (IO.bufRaw buf) (\ptr ->
					Data.ByteString.packCStringLen (bufStart ptr, bufLen))
				let newBytes = Data.ByteString.concat [before, padding, bufBytes, Data.ByteString.drop bufLen after]
				return (pos + bufLen, newBytes)
		return (buf { IO.bufL = 0, IO.bufR = 0 })
	
	flushWriteBuffer0 dev buf = do
		newBuf <- IO.flushWriteBuffer dev buf
		return (IO.bufR buf - IO.bufL buf, newBuf)

newKnob :: MonadIO m => ByteString -> m Knob
newKnob bytes = do
	var <- liftIO (MVar.newMVar bytes)
	return (Knob var)

getContents :: MonadIO m => Knob -> m ByteString
getContents (Knob var) = liftIO (MVar.readMVar var)

setContents :: MonadIO m => Knob -> ByteString -> m ()
setContents (Knob var) bytes = liftIO (MVar.modifyMVar_ var (\_ -> return bytes))

-- | Create a new 'IO.Handle' pointing to a 'Knob'. This handle behaves like
-- a file-backed handle for most purposes.
newFileHandle :: MonadIO m
              => Knob
              -> String -- ^ Filename shown in error messages
              -> IO.IOMode -> m IO.Handle
newFileHandle (Knob var) name mode = liftIO $ do
	startPosition <- MVar.withMVar var $ \bytes -> return $ case mode of
		IO.AppendMode -> Data.ByteString.length bytes
		_ -> 0
	posVar <- MVar.newMVar startPosition
	IO.mkFileHandle (Device mode var posVar) name mode Nothing IO.noNewlineTranslation

-- | See 'newFileHandle'.
withFileHandle :: MonadIO m
               => Knob
               -> String -- ^ Filename shown in error messages.
               -> IO.IOMode -> (IO.Handle -> IO a) -> m a
withFileHandle knob name mode io = liftIO (bracket (newFileHandle knob name mode) IO.hClose io)
