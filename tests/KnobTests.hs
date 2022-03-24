{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString
import           Data.ByteString.Char8 ()
import           Data.ByteString.Unsafe (unsafePackCStringLen)
import           Foreign (nullPtr)
import qualified GHC.IO.Exception as GHC
import           System.IO
import           Test.Chell

import           Data.Knob

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
tests = [test_File, test_Duplex]

test_File :: Suite
test_File = suite "file"
	[ suite "read"
		[ test_ReadFromStart
		, test_ReadFromOffset
		, test_ReadToEOF
		, test_ReadPastEOF
		]
	, suite "write"
		[ test_WriteFromStart
		, test_WriteFromOffset
		, test_WritePastEOF
		, test_WriteAppended
		]
	, suite "seek"
		[ test_SeekAbsolute
		, test_SeekRelative
		, test_SeekFromEnd
		, test_SeekBeyondMaxInt
		]
	, suite "setSize"
		[ test_SetSize_Read
		, test_SetSize_Write
		, test_SetSize_ReadWrite
		, test_SetSize_Append
		]
	
	, test_Ready
	, test_Close
	, test_SetContents
	, test_WithFileHandle
	]

test_ReadFromStart :: Suite
test_ReadFromStart = assertions "from-start" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	bytes <- liftIO $ Data.ByteString.hGet h 3
	$expect (equal bytes "abc")
	
	off <- liftIO $ hTell h
	$expect (equal off 3)

test_ReadFromOffset :: Suite
test_ReadFromOffset = assertions "from-offset" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	liftIO $ hSeek h AbsoluteSeek 1
	bytes <- liftIO $ Data.ByteString.hGet h 3
	$expect (equal bytes "bcd")
	
	off <- liftIO $ hTell h
	$expect (equal off 4)

test_ReadToEOF :: Suite
test_ReadToEOF = assertions "to-eof" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	bytes <- liftIO $ Data.ByteString.hGet h 10
	$expect (equal bytes "abcde")
	
	off <- liftIO $ hTell h
	$expect (equal off 5)

test_ReadPastEOF :: Suite
test_ReadPastEOF = assertions "past-eof" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	liftIO $ hSeek h AbsoluteSeek 10
	bytes <- liftIO $ Data.ByteString.hGet h 10
	$expect (equal bytes "")
	
	off <- liftIO $ hTell h
	$expect (equal off 10)

test_WriteFromStart :: Suite
test_WriteFromStart = assertions "from-start" $ do
	k <- newKnob ""
	h <- newFileHandle k "foo.txt" WriteMode
	liftIO $ hSetBuffering h NoBuffering
	
	liftIO $ Data.ByteString.hPut h "abcde"
	bytes <- Data.Knob.getContents k
	$expect (equal bytes "abcde")

test_WriteFromOffset :: Suite
test_WriteFromOffset = assertions "from-offset" $ do
	k <- newKnob ""
	h <- newFileHandle k "foo.txt" WriteMode
	liftIO $ hSetBuffering h NoBuffering
	
	liftIO $ Data.ByteString.hPut h "abcde"
	liftIO $ hSeek h AbsoluteSeek 2
	liftIO $ Data.ByteString.hPut h "abcde"
	
	bytes <- Data.Knob.getContents k
	$expect (equal bytes "ababcde")

test_WritePastEOF :: Suite
test_WritePastEOF = assertions "past-eof" $ do
	k <- newKnob ""
	h <- newFileHandle k "foo.txt" WriteMode
	liftIO $ hSetBuffering h NoBuffering
	
	liftIO $ hSeek h AbsoluteSeek 2
	liftIO $ Data.ByteString.hPut h "abcde"
	bytes <- Data.Knob.getContents k
	$expect (equal bytes "\0\0abcde")

test_WriteAppended :: Suite
test_WriteAppended = assertions "appended" $ do
	k <- newKnob "foo"
	h <- newFileHandle k "foo.txt" AppendMode
	liftIO $ hSetBuffering h NoBuffering
	
	liftIO $ Data.ByteString.hPut h "bar"
	bytes <- Data.Knob.getContents k
	$expect (equal bytes "foobar")

test_SeekAbsolute :: Suite
test_SeekAbsolute = assertions "absolute" $ do
	k <- newKnob ""
	h <- newFileHandle k "foo.txt" ReadMode
	
	before <- liftIO $ hTell h
	liftIO $ hSeek h AbsoluteSeek 2
	after <- liftIO $ hTell h
	
	$expect (equal before 0)
	$expect (equal after 2)

test_SeekRelative :: Suite
test_SeekRelative = assertions "relative" $ do
	k <- newKnob ""
	h <- newFileHandle k "foo.txt" ReadMode
	
	before <- liftIO $ hTell h
	liftIO $ hSeek h RelativeSeek 2
	after1 <- liftIO $ hTell h
	liftIO $ hSeek h RelativeSeek 2
	after2 <- liftIO $ hTell h
	
	$expect (equal before 0)
	$expect (equal after1 2)
	$expect (equal after2 4)

test_SeekFromEnd :: Suite
test_SeekFromEnd = assertions "from-end" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	before <- liftIO $ hTell h
	liftIO $ hSeek h SeekFromEnd (- 2)
	after <- liftIO $ hTell h
	
	$expect (equal before 0)
	$expect (equal after 3)

test_SeekBeyondMaxInt :: Suite
test_SeekBeyondMaxInt = assertions "beyond-max-int" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	let intPlusOne = toInteger (maxBound :: Int) + 1
	
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSeek" "offset > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSeek h AbsoluteSeek intPlusOne)
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSeek" "offset > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSeek h RelativeSeek intPlusOne)
	
	-- testing this with real contents is difficult/impossible on a
	-- 64-bit system, so use an unsafe function to corrupt the knob's
	-- internal buffer first.
	hugeBytes <- liftIO (unsafePackCStringLen (nullPtr, maxBound))
	liftIO $ hSeek h AbsoluteSeek (intPlusOne - 1)
	setContents k hugeBytes
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSeek" "offset > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSeek h SeekFromEnd 2)

test_Ready :: Suite
test_Ready = assertions "ready" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	ready <- liftIO $ hReady h
	$expect ready
	
	_ <- liftIO $ Data.ByteString.hGet h 10
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.EOF "hWaitForInput" "" Nothing (Just "foo.txt"))
		(hReady h)

test_Close :: Suite
test_Close = assertions "close" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	liftIO $ hClose h
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.IllegalOperation "hGetBuf" "handle is closed" Nothing (Just "foo.txt"))
		(Data.ByteString.hGet h 1)
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.IllegalOperation "hWaitForInput" "handle is closed" Nothing (Just "foo.txt"))
		(hReady h)

test_SetSize_Read :: Suite
test_SetSize_Read = assertions "ReadMode" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadMode
	
	let intPlusOne = toInteger (maxBound :: Int) + 1
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSetFileSize" "size > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSetFileSize h intPlusOne)
	
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.IllegalOperation "hSetFileSize" "handle in ReadMode" Nothing (Just "foo.txt"))
		(hSetFileSize h 2)

test_SetSize_Write :: Suite
test_SetSize_Write = assertions "WriteMode" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" WriteMode
	
	let intPlusOne = toInteger (maxBound :: Int) + 1
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSetFileSize" "size > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSetFileSize h intPlusOne)
	
	-- Resets contents to all NULL, regardless of offset
	liftIO $ hSeek h AbsoluteSeek 2
	liftIO $ hSetFileSize h 4
	
	bytes <- Data.Knob.getContents k
	$expect (equal bytes "\0\0\0\0")

test_SetSize_ReadWrite :: Suite
test_SetSize_ReadWrite = assertions "ReadWriteMode" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" ReadWriteMode
	
	let intPlusOne = toInteger (maxBound :: Int) + 1
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSetFileSize" "size > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSetFileSize h intPlusOne)
	
	-- Truncates contents, regardless of offset
	do
		liftIO $ hSeek h AbsoluteSeek 2
		liftIO $ hSetFileSize h 4
		bytes <- Data.Knob.getContents k
		$expect (equal bytes "abcd")
	
	do
		liftIO $ hSetFileSize h 6
		bytes <- Data.Knob.getContents k
		$expect (equal bytes "abcd\0\0")

test_SetSize_Append :: Suite
test_SetSize_Append = assertions "AppendMode" $ do
	k <- newKnob "abcde"
	h <- newFileHandle k "foo.txt" AppendMode
	
	let intPlusOne = toInteger (maxBound :: Int) + 1
	$expect $ throwsEq
		(GHC.IOError (Just h) GHC.InvalidArgument "hSetFileSize" "size > (maxBound :: Int)" Nothing (Just "foo.txt"))
		(hSetFileSize h intPlusOne)
	
	do
		liftIO $ hSetFileSize h 4
		bytes <- Data.Knob.getContents k
		$expect (equal bytes "abcd")
	
	do
		liftIO $ hSetFileSize h 6
		bytes <- Data.Knob.getContents k
		$expect (equal bytes "abcd\0\0")

test_SetContents :: Suite
test_SetContents = assertions "setContents" $ do
	k <- newKnob "abcde"
	before <- Data.Knob.getContents k
	setContents k "foo"
	after <- Data.Knob.getContents k
	
	$expect (equal before "abcde")
	$expect (equal after "foo")

test_WithFileHandle :: Suite
test_WithFileHandle = assertions "withFileHandle" $ do
	k <- newKnob ""
	h <- withFileHandle k "test.txt" WriteMode $ \h -> do
		Data.ByteString.hPut h "abcde"
		return h
	
	bytes <- Data.Knob.getContents k
	$expect (equal bytes "abcde")
	
	closed <- liftIO $ hIsClosed h
	$expect closed

test_Duplex :: Suite
test_Duplex = suite "duplex" []
