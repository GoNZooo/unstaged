module Main where

import qualified Library
import Options.Applicative
import Prelude

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = do
  let parseOptions =
        info
          (optionsP <**> helper)
          (fullDesc <> progDesc programDescription <> header ("unstaged - " <> programDescription))
      programDescription = "A tool for printing git repository statuses"
  options <- execParser parseOptions
  Library.runMain options

optionsP :: Parser Library.Options
optionsP =
  Library.Options <$> argument str (metavar "PATH")
    <*> option
      (maybeReader maybeGitStatusType)
      ( long "status-type" <> short 't' <> help "Which git status to filter for"
          <> metavar "STATUSTYPE"
          <> value Library.UnstagedStatus
      )

maybeGitStatusType :: String -> Maybe Library.GitStatusType
maybeGitStatusType "unstaged" = Just Library.UnstagedStatus
maybeGitStatusType "no-commits" = Just Library.NoCommitsStatus
maybeGitStatusType "clean" = Just Library.CleanStatus
maybeGitStatusType _otherwise = Nothing
