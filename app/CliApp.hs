module CliApp (
  addPattern
  , removePattern
  , printOut
)
where

import Control.Monad
import Data.List (delete)
import Text.Read (readMaybe)
import Gitignore (
  PlainPattern(..)
  , MonadCanWriteToStdOut
  , MonadCanReadFromStdIn
  , CanAddPattern
  , CanRemovePattern
  , CanReadAndPrintContent
  , readGitignoreContent
  , checkLastCharIsNewLine
  , appendToGitignore
  , readFromStdIn
  , writeToStdOut
  , writeToStdErr
  , writeToTempGitignore
  , removeGitignore
  , renameFileToGitignore,
  ) 

addPattern :: (CanAddPattern m) => PlainPattern -> m ()
addPattern (PlainPattern pattern) = do
  content <- readGitignoreContent
  let w = words content
  if pattern `elem` w
    then writeToStdErr "Error: pattern already present in .gitignore"
    else do
      yes <- checkLastCharIsNewLine
      unless yes $ appendToGitignore "\n"
      appendToGitignore pattern

removePattern :: (CanRemovePattern m) => m ()
removePattern = do
  content <- readGitignoreContent
  let contentLines = lines content
      numberedLines = zipWith (\n line -> show (n :: Int) <> ": " <> line) [1 ..] contentLines
  writeToStdOut "Which pattern would you like to remove? (0 to abort)"
  writeToStdOut $ unlines numberedLines
  number <- readIntoNumber
  if number == 0
    then
      pure ()
    else do
      let newContentLines = unlines $ delete (contentLines !! (number - 1)) contentLines
      fileName <- writeToTempGitignore newContentLines
      removeGitignore
      renameFileToGitignore fileName

printOut :: (CanReadAndPrintContent m) => m ()
printOut = do
  content <- readGitignoreContent
  writeToStdOut content




--- Helper functions ---

readIntoNumber :: (Read a, MonadCanReadFromStdIn m, MonadCanWriteToStdOut m) => m a
readIntoNumber = do
  numberString <- readFromStdIn
  case readMaybe numberString of
    Just int -> pure int
    Nothing -> do
      writeToStdOut "Invalid input. Please enter a number."
      readIntoNumber