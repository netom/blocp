{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent.STM.TBChan
import qualified Data.ByteString as B
import           Lib
import           Options.Applicative
import           System.IO

-- TODO Features:
--
-- Tunable read, write and compare block sizes
-- Offset / length (e.g. for copying partitions)
-- Ignore size mismatch target
-- Append data to the target
-- Synchronous writes
-- Periodic sync
-- Retries on read/write errors
-- Failure modes on errors
--   Fail fast
--   Retry N times
--   Retry quickly or later
--   Write error log

-- Check: file existance
-- Check: files must be of the same size

defaultBlockSize :: Int
defaultBlockSize = 4096

data Options = Options
    { optSource :: String
    , optTarget :: String
    , optBlockSize :: Int
    , optIgnoreLengthMismatch :: Bool
    , optAppend    :: Bool
    , optTruncate  :: Bool
    , optSync :: Bool
    } deriving Show

options :: ParserInfo Options
options = info
    (helper <*> (
        Options
            <$> (  strArgument
                $  metavar "SOURCE"
                <> help "The source file."
                )
            <*> (  strArgument
                $  metavar "TARGET"
                <> help "The target file."
                )
            <*> (  option auto
                $  long "block-size"
                <> short 'b'
                <> metavar "BLOCKSIZE"
                <> value defaultBlockSize
                <> help
                    (  "The size of blocks read and written in bytes. The default is "
                    ++ show defaultBlockSize
                    ++ "."
                    )
                )
            <*> (  switch 
                $ long "ignore-length-mismatch"
                <> short 'l'
                <> help "Don't abort if the source and target size does not match."
                )
            <*> (  switch 
                $ long "append"
                <> short 'a'
                <> help "If the target is smaller than the source, append data to it."
                )
            <*> (  switch 
                $ long "truncate"
                <> short 't'
                <> help "Truncate the target if it is larger than the source."
                )
            <*> (  switch 
                $ long "sync"
                <> short 's'
                <> help "Use O_SYNC when opening the target."
                )
    )) (
        fullDesc
            <> progDesc "Copy modified blocks between large files or block devices."
    )

type BufChan = TBChan B.ByteString

wWriter :: Handle -> BufChan -> IO ()
wWriter _ _ = return ()

wReader :: Handle -> BufChan -> IO ()
wReader _ _ = return ()

wCmp :: BufChan -> BufChan -> BufChan -> IO ()
wCmp _ _ _ = return ()

main :: IO ()
main = do
    Options{..} <- execParser options

    chSourceRead  <- newTBChanIO 10
    chTargetRead  <- newTBChanIO 10
    chTargetWrite <- newTBChanIO 10

    hSource <- openBinaryFile optSource ReadMode
    hTarget <- openBinaryFile optTarget ReadWriteMode

    wReader hSource chSourceRead
    wReader hTarget chTargetRead
    wWriter hTarget chTargetWrite
    wCmp chSourceRead chTargetRead chTargetWrite