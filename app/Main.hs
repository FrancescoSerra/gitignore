{-# LANGUAGE LambdaCase #-}

module Main where

import CliOptions
import CliApp ( addPattern, removePattern, printOut )
import Control.Monad (unless)
import Gitignore ( checkGitignoreExists, createGitignore )
import System.Exit (exitFailure)

main :: IO ()
main = do
  exists <- checkGitignoreExists
  unless exists confirmAndCreate
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
      addPattern plainPattern
    Remove -> do
      removePattern
    Print -> do
      printOut