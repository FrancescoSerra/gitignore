{-# LANGUAGE LambdaCase #-}

module Main where

-- import GHC.IO.Exception (ExitCode (ExitSuccess))
-- import System.Process

import CliOptions
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Gitignore
import GHC.IO.StdHandles
import GHC.IO.IOMode


main :: IO ()
main = do
  exists <- doesFileExist ".gitignore"
  shouldOpenFile <-
    if not exists
      then confirm
      else pure True
  if shouldOpenFile
    then processAction
    else
      exitFailure
  putStrLn "Done!"

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn ".gitignore doesn't exist in the current directory. Do you want to create it? (y/n)"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Invalid response. use y or n"
          *> confirm

processAction :: IO ()
processAction = do
        commands <- parse
        case commands of
          Add (SinglePattern plainPattern) -> do
            addPattern plainPattern ".gitignore"
          
          Remove -> do
            removePattern ".gitignore"
          
          Print -> do
            withFile ".gitignore" ReadMode printOut