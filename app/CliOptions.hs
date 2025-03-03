module CliOptions where

import qualified Gitignore as G
import Options.Applicative

data Commands
  = Add SinglePattern
  | Remove SinglePattern

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

pRemoveSingle :: Parser Commands
pRemoveSingle =
  Remove <$> pSinglePattern

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
              (helper <*> pRemoveSingle)
              (progDesc "Remove a single pattern from .gitignore")
          )
    )

comms :: ParserInfo Commands
comms =
  info
    (helper <*> pCommands)
    ( fullDesc
        <> header "gitignore - a utility to manipulate .gitignore in a git repository"
        <> progDesc "Add patterns to your project's .gitignore"
    )

parse :: IO Commands
parse = execParser comms