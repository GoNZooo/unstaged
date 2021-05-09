module Library where

import RIO
import RIO.ByteString (ByteString)
import qualified RIO.ByteString as ByteString
import qualified RIO.ByteString.Lazy as LazyByteString
import System.IO (putStrLn)
import qualified System.Process.Typed as Process

data GitStatus
  = UnstagedChanges
  | NotGitRepository
  | Clean
  | NoCommits
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

runMain :: IO ()
runMain = do
  putStrLn "Hello, World!"

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
