module Gitignore where

import Data.List (delete)
import System.Directory
import System.IO

newtype PlainPattern = PlainPattern String

addPattern :: PlainPattern -> FilePath -> IO ()
addPattern (PlainPattern pattern) file = do
  content <- readFile file
  let w = words content
   in if pattern `elem` w
        then
          hPutStrLn stderr "Error: pattern already present in .gitignore"
        else
          appendFile file pattern

removePattern :: FilePath -> IO ()
removePattern file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  let contentLines = lines content
      numberedLines = zipWith (\n line -> show (n :: Int) ++ ": " ++ line) [1 ..] contentLines
  putStrLn "Which pattern would you like to remove? (0 to abort)"
  putStr $ unlines numberedLines
  numberString <- getLine
  let number = read numberString
  if number == 0
    then do
      hClose handle
    else do
      let newContentLines = delete (contentLines !! (number - 1)) contentLines
      (tempName, tempHandle) <- openTempFile "." ".gitignore~"
      hPutStr tempHandle $ unlines newContentLines
      hClose handle
      hClose tempHandle
      removeFile file
      renameFile tempName file

printOut :: Handle -> IO ()
printOut file = do
  content <- hGetContents file
  putStrLn content
