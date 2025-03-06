{-# LANGUAGE LambdaCase #-}

module Main where

import CliOptions
import System.Exit (exitFailure)
import Gitignore
import GHC.IO.StdHandles
import GHC.IO.IOMode


main :: IO ()
main = do
  exists <- checkGitignoreExists
  if not exists
  then confirmAndCreate
  else pure ()
  processAction
  putStrLn "Done!"

-- | Confirm user action
confirmAndCreate :: IO ()
confirmAndCreate =
  putStrLn ".gitignore doesn't exist in the current directory. Do you want to create it? (y/n)"
    *> getLine
    >>= \case
      "y" -> createGitignore
      "n" -> exitFailure
      _ ->
        putStrLn "Invalid response. use y or n"
          *> confirmAndCreate

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