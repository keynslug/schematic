{-# LANGUAGE Rank2Types #-}

module Data.FileStore.GitSync (
      module Data.FileStore.Internal.Types
    , SyncFileStore(..)
    , fileStore
    ) where

import Data.FileStore.Internal.Types

import qualified Data.FileStore as FS

import Data.FileStore.Git
import Data.FileStore.Utils (runShellCommand)

import Control.Exception (throwIO)
import System.Exit (ExitCode(..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)

data SyncFileStore = SyncFileStore {

      save           :: Contents a => FilePath -> Author -> Description -> a -> IO ()
    , retrieve       :: Contents a => FilePath -> Maybe RevisionId -> IO a
    , delete         :: FilePath -> Author -> Description -> IO ()
    , rename         :: FilePath -> FilePath -> Author -> Description -> IO ()
    , history        :: [FilePath] -> TimeRange -> Maybe Int -> IO [Revision]
    , latest         :: FilePath -> IO RevisionId
    , revision       :: RevisionId -> IO Revision
    , index          :: IO [FilePath]
    , directory      :: FilePath -> IO [Resource]
    , idsMatch       :: RevisionId -> RevisionId -> Bool
    , search         :: SearchQuery -> IO [SearchMatch]

    , sync           :: IO RevisionId

    }

fileStore :: FilePath -> SyncFileStore
fileStore repo =
    let fs = gitFileStore repo in SyncFileStore {
          save              = FS.save       fs
        , retrieve          = FS.retrieve   fs
        , delete            = FS.delete     fs
        , rename            = FS.rename     fs
        , history           = FS.history    fs
        , latest            = FS.latest     fs
        , revision          = FS.revision   fs
        , index             = FS.index      fs
        , directory         = FS.directory  fs
        , search            = FS.search     fs
        , idsMatch          = FS.idsMatch   fs
        , sync              = pullPush repo fs
        }

runGitCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, ByteString)
runGitCommand repo command args = do
    (status, err, out) <- runShellCommand repo Nothing "git" (command : args)
    return (status, toString err, out)

ensureGitCommand :: FilePath -> String -> [String] -> IO ()
ensureGitCommand repo command args =
    runGitCommand repo command args >>= handle where
        handle (ExitSuccess, _, _) = return ()
        handle (ExitFailure _, err, _) = throwIO $ UnknownError err

pullPush :: FilePath -> FS.FileStore -> IO RevisionId
pullPush repo fs = do
    _ <- ensureGitCommand repo "pull" ["--rebase"]
    _ <- ensureGitCommand repo "push" []
    FS.latest fs ""
