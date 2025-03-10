module Gitignore.Internal where

import System.Directory
import System.IO
import System.IO.Temp

-- typeclasses --
class (Monad m) => MonadCheckFileExistence m where
  checkFileExists :: FilePath -> m Bool

class (Monad m) => MonadAppendFile m where
  appendToFile :: FilePath -> String -> m ()

class (Monad m) => MonadReadContent m where
  readContent :: FilePath -> m String

class (Monad m) => MonadCheckLastCharIsNewLine m where
  checkIfNewLineEnding :: FilePath -> m Bool

class (Monad m) => MonadCanWriteToStdErr m where
  writeToStdErr :: String -> m ()

class (Monad m) => MonadCanWriteToStdOut m where
  writeToStdOut :: String -> m ()

class (Monad m) => MonadCanReadFromStdIn m where
  readFromStdIn :: m String

class (Monad m) => MonadCanWriteToTempFile m where
  writeToTempFile :: FilePath -> String -> String -> m FilePath

class (Monad m) => MonadCanDeleteFile m where
  deleteFile :: FilePath -> m ()

class (Monad m) => MonadCanRenameFile m where
  changeFileName :: FilePath -> FilePath -> m ()

class
  (MonadReadContent m, MonadCanWriteToStdErr m, MonadCheckLastCharIsNewLine m, MonadAppendFile m) =>
  CanAddPattern m

class
  (MonadReadContent m, MonadCanWriteToStdOut m) =>
  CanReadAndPrintContent m

class (
  MonadReadContent m, 
  MonadCanWriteToStdOut m, 
  MonadCanReadFromStdIn m, 
  MonadCanWriteToTempFile m,
  MonadCanDeleteFile m,
  MonadCanRenameFile m
  ) => CanRemovePattern m

-- IO instances --
instance MonadCheckFileExistence IO where
  checkFileExists = doesFileExist

instance MonadAppendFile IO where
  appendToFile = appendFile

instance MonadCheckLastCharIsNewLine IO where
  checkIfNewLineEnding filePath = do
    content <- readFile filePath
    pure (last content == '\n')

instance MonadCanWriteToStdErr IO where
  writeToStdErr = hPutStrLn stderr

instance MonadCanWriteToStdOut IO where
  writeToStdOut = hPutStrLn stdout

instance MonadReadContent IO where
  readContent = readFile

instance MonadCanReadFromStdIn IO where
  readFromStdIn = getLine

instance MonadCanWriteToTempFile IO where
  writeToTempFile = writeTempFile

instance MonadCanDeleteFile IO where
  deleteFile = removeFile

instance MonadCanRenameFile IO where
  changeFileName = renameFile

instance CanAddPattern IO
instance CanReadAndPrintContent IO
instance CanRemovePattern IO