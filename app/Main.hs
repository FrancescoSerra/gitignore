module Main where

-- import GHC.IO.Exception (ExitCode (ExitSuccess))
-- import System.Process
import CliOptions
import qualified Gitignore as G

main :: IO ()
main = do
  commands <- parse
  case commands of
    Add (SinglePattern (G.PlainPattern str)) ->
      putStrLn $ "Adding single pattern " <> str <> "!"
    Remove (SinglePattern (G.PlainPattern str)) ->
      putStrLn $ "Removing single pattern " <> str <> "!"

  -- ExitSuccess <- system "echo '/foo' >> .gitignore"
  putStrLn "Done!"
