module Library where

import RIO
import qualified RIO.ByteString as ByteString
import qualified RIO.ByteString.Lazy as LazyByteString
import qualified RIO.Directory as Directory
import qualified RIO.List as List
import System.IO (putStrLn)
import qualified System.Process.Typed as Process

data GitStatus
  = UnstagedChanges FilePath
  | NotGitRepository
  | Clean FilePath
  | NoCommits FilePath
  | UnknownStatus FilePath ProcessOutput
  deriving (Eq, Show)

data ProcessOutput = ProcessOutput
  { standardOut :: OutputBytes,
    standardError :: ErrorBytes
  }
  deriving (Eq, Show)

newtype OutputBytes = OutputBytes ByteString
  deriving (Eq, Show)

newtype ErrorBytes = ErrorBytes ByteString
  deriving (Eq, Show)

newtype WorkingDirectory = WorkingDirectory FilePath
  deriving (Eq, Show)

newtype CommandString = CommandString String
  deriving (Eq, Show)

data Options = Options {path :: FilePath, statusType :: GitStatusType}

data GitStatusType
  = UnstagedStatus
  | CleanStatus
  | NoCommitsStatus

runMain :: Options -> IO ()
runMain Options {path, statusType} = do
  gitStatuses <- getGitStatuses path
  let repositories =
        foldr
          (folder statusType)
          []
          gitStatuses
          & List.sort
      folder UnstagedStatus (UnstagedChanges p) pathsSoFar = p : pathsSoFar
      folder UnstagedStatus _otherwise pathsSoFar = pathsSoFar
      folder NoCommitsStatus (NoCommits p) pathsSoFar = p : pathsSoFar
      folder NoCommitsStatus _otherwise pathsSoFar = pathsSoFar
      folder CleanStatus (Clean p) pathsSoFar = p : pathsSoFar
      folder CleanStatus _otherwise pathsSoFar = pathsSoFar
  forM_ repositories putStrLn

getGitStatuses :: FilePath -> IO [GitStatus]
getGitStatuses path = do
  localGitStatus <- getGitStatus path
  case localGitStatus of
    Clean _path -> pure [Clean path]
    UnstagedChanges _path -> pure [UnstagedChanges path]
    NoCommits _path -> pure [NoCommits path]
    unknown@(UnknownStatus _ _) -> pure [unknown]
    NotGitRepository -> do
      subDirectories <- getSubDirectories path
      foldMap getGitStatuses subDirectories

getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories path = do
  files <- Directory.listDirectory path
  let completeFilePaths = fmap (path </>) files
  filterM Directory.doesDirectoryExist completeFilePaths

(</>) :: FilePath -> FilePath -> FilePath
p1 </> p2 = dropEndingSlashes p1 <> "/" <> p2

dropEndingSlashes :: FilePath -> FilePath
dropEndingSlashes = List.dropWhileEnd (== '/')

getGitStatus :: FilePath -> IO GitStatus
getGitStatus path = do
  processOutput@ProcessOutput
    { standardOut = (OutputBytes outBytes),
      standardError = (ErrorBytes errorBytes)
    } <-
    getProcessOutput (WorkingDirectory path) (CommandString "git status")
  if
      | "Changes not staged for commit" `ByteString.isInfixOf` outBytes ->
        pure $ UnstagedChanges path
      | "No commits yet" `ByteString.isInfixOf` outBytes ->
        pure $ NoCommits path
      | "fatal: not a git repository (or any of the parent directories): .git"
          `ByteString.isInfixOf` errorBytes ->
        pure NotGitRepository
      | "nothing to commit, working tree clean" `ByteString.isInfixOf` outBytes ->
        pure $ Clean path
      | otherwise -> pure (UnknownStatus path processOutput)

getProcessOutput :: WorkingDirectory -> CommandString -> IO ProcessOutput
getProcessOutput (WorkingDirectory workingDirectory) (CommandString commandString) = do
  case words commandString of
    command : arguments -> do
      let processConfiguration =
            Process.proc command arguments
              & Process.setStdout Process.byteStringOutput
              & Process.setStderr Process.byteStringOutput
              & Process.setWorkingDir workingDirectory
      Process.withProcessWait processConfiguration $ \process -> atomically $ do
        outBytes <- Process.getStdout process
        errorBytes <- Process.getStderr process
        let standardOut = outBytes & LazyByteString.toStrict & OutputBytes
            standardError = errorBytes & LazyByteString.toStrict & ErrorBytes
        pure ProcessOutput {standardOut, standardError}
    [] ->
      error "Empty command string"
