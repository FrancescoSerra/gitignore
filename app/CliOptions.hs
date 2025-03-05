module CliOptions where

import qualified Gitignore as G
import Options.Applicative

data Commands
  = Add SinglePattern
  | Remove
  | Print

newtype SinglePattern
  = SinglePattern G.PlainPattern

pPattern :: Parser G.PlainPattern
pPattern =
  G.PlainPattern
    <$> strArgument
      ( metavar "STRING"
          <> help "Pattern"
      )

pSinglePattern :: Parser SinglePattern
pSinglePattern =
  SinglePattern <$> pPattern

pAddSingle :: Parser Commands
pAddSingle =
  Add <$> pSinglePattern

pCommands :: Parser Commands
pCommands =
  subparser
    ( command
        "add"
        ( info
            (helper <*> pAddSingle)
            (progDesc "Add a single pattern to .gitignore")
        )
        <> command
          "remove"
          ( info
              (helper <*> pure Remove)
              (progDesc "Remove a single pattern from .gitignore")
          )
        <> command
          "print"
          ( info 
            (helper <*> pure Print)
            (progDesc "Print .gitignore's content")
          )
    )

comms :: ParserInfo Commands
comms =
  info
    (helper <*> pCommands)
    ( fullDesc
        <> header "gitignore - a utility to manipulate .gitignore in a git repository"
        <> progDesc "Add/change/remove patterns to your project's .gitignore"
    )

parse :: IO Commands
parse = execParser comms