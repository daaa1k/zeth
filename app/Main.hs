module Main where

import Options.Applicative
import Zettelkasten

data Options = Options
    { filename :: Maybe String }

optionsParser :: Parser Options
optionsParser = Options
    <$> optional (argument str (metavar "FILENAME"))

main :: IO ()
main = do
    opts <- execParser optsParser
    zet (filename opts)
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Create and open zettel notes"
     <> header "zet - a Zettelkasten-style note utility" )
