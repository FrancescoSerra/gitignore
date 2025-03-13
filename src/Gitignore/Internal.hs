{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Gitignore.Internal where

import Control.Monad
import Data.Functor.Identity
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

-- Identity instances --
instance MonadCheckFileExistence Identity where
  checkFileExists _ = pure True

instance MonadAppendFile Identity where
  appendToFile _ _ = pure ()

instance MonadCheckLastCharIsNewLine Identity where
  checkIfNewLineEnding _ = pure True

instance MonadCanWriteToStdErr Identity where
  writeToStdErr _ = pure ()

instance MonadCanWriteToStdOut Identity where
  writeToStdOut _ = pure ()

instance MonadReadContent Identity where
  readContent _ = pure ""

instance MonadCanReadFromStdIn Identity where
  readFromStdIn = pure "1"

instance MonadCanWriteToTempFile Identity where
  writeToTempFile _ _ _ = pure ""

instance MonadCanDeleteFile Identity where
  deleteFile _ = pure ()

instance MonadCanRenameFile Identity where
  changeFileName _ _ = pure ()

instance CanAddPattern Identity

instance CanReadAndPrintContent Identity

instance CanRemovePattern Identity

-- Either instances --


data Computation a = Error String | Result a
  deriving (Functor, Show, Eq)

instance Applicative Computation where
  pure = Result
  (<*>) :: Computation (a -> b) -> Computation a -> Computation b
  (<*>) = ap

instance Monad Computation where
  (Result x)  >>= k   =  k x
  (Error a)   >>= _   =  Error a

instance MonadCanReadFromStdIn Computation where
  readFromStdIn = pure "boom!"

instance MonadCanWriteToStdOut Computation where
  writeToStdOut = Error