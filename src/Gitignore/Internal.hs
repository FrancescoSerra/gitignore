{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Gitignore.Internal where

import Control.Monad
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

class
  ( MonadReadContent m,
    MonadCanWriteToStdOut m,
    MonadCanReadFromStdIn m,
    MonadCanWriteToTempFile m,
    MonadCanDeleteFile m,
    MonadCanRenameFile m
  ) =>
  CanRemovePattern m

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
  writeToStdOut = putStrLn

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

-- Computation instances --

data Computation a = Error String | Result a
  deriving (Functor, Show, Eq)

instance Applicative Computation where
  pure = Result
  (<*>) :: Computation (a -> b) -> Computation a -> Computation b
  (<*>) = ap

instance Monad Computation where
  (Result x) >>= k = k x
  (Error a) >>= _ = Error a

instance MonadCanReadFromStdIn Computation where
  readFromStdIn = pure "1"

instance MonadCanWriteToStdOut Computation where
  writeToStdOut _ = pure ()

instance MonadCanWriteToStdErr Computation where
  writeToStdErr _ = pure ()

instance MonadCanWriteToTempFile Computation where
  writeToTempFile filePath _ _ = pure filePath

instance MonadCanDeleteFile Computation where
  deleteFile _ = pure ()

instance MonadCanRenameFile Computation where
  changeFileName _ _ = pure ()

instance MonadCheckFileExistence Computation where
  checkFileExists _ = pure True

instance MonadAppendFile Computation where
  appendToFile _ _ = pure ()

instance MonadCheckLastCharIsNewLine Computation where
  checkIfNewLineEnding _ = pure True

instance MonadReadContent Computation where
  readContent _ = pure "content"

instance CanAddPattern Computation

instance CanReadAndPrintContent Computation

instance CanRemovePattern Computation

-- FailedComputation instances --

newtype FailedComputation a = FailedComputation (Computation a)
  deriving (Show, Eq, Functor)

instance Applicative FailedComputation where
  pure = FailedComputation . pure
  (<*>) :: FailedComputation (a -> b) -> FailedComputation a -> FailedComputation b
  (<*>) = ap

instance Monad FailedComputation where
    (FailedComputation (Result x)) >>= k = k x
    (FailedComputation (Error a)) >>= _ = FailedComputation $ Error a

instance MonadCanReadFromStdIn FailedComputation where
  readFromStdIn = pure "boom!"

instance MonadCanWriteToStdOut FailedComputation where
  writeToStdOut s = FailedComputation $ Error s

instance MonadCanWriteToStdErr FailedComputation where
  writeToStdErr s = FailedComputation $ Error s

instance MonadCanWriteToTempFile FailedComputation where
  writeToTempFile filePath _ _ = FailedComputation $ Error filePath

instance MonadCanDeleteFile FailedComputation where
  deleteFile s = FailedComputation $ Error s

instance MonadCanRenameFile FailedComputation where
  changeFileName filePath _ = FailedComputation $ Error filePath

instance MonadCheckFileExistence FailedComputation where
  checkFileExists filePath = FailedComputation $ Error filePath

instance MonadAppendFile FailedComputation where
  appendToFile filePath _ = FailedComputation $ Error filePath

instance MonadCheckLastCharIsNewLine FailedComputation where
  checkIfNewLineEnding filePath = FailedComputation $ Error filePath

instance MonadReadContent FailedComputation where
  readContent filePath = FailedComputation $ Error filePath

instance CanAddPattern FailedComputation

instance CanReadAndPrintContent FailedComputation

instance CanRemovePattern FailedComputation
