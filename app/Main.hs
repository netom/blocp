module Main where

import Lib
import Options.Applicative

data Options = Options
    { optSource :: String
    , optTarget :: String
    , optIgnoreLengthMismatch :: Bool
    } deriving Show

options :: ParserInfo Options
options = info
    (helper <*> (
        Options
            <$> (  strOption
                $  long "source"
                <> short 's'
                <> metavar "SOURCE"
                <> help "The source file."
                )
            <*> (  strOption
                $  long "target"
                <> short 't'
                <> metavar "TARGET"
                <> help "The target file."
                )
            <*> (  switch 
                $ long "ignore-length-mismatch"
                <> short 'l'
                <> help "Don't abort if the source and target size does not match."
                )
    )) (
        fullDesc
            <> progDesc "Copy modified blocks between large files or block devices."
    )

main :: IO ()
main = do
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
    _ <- execParser options
    putStrLn "TODO"