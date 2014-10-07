module Schemata (Name, list, fetchData, fetch, update) where

import Control.Exception

import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.FileStore.GitSync

import System.FilePath ((</>), (<.>))

type Name = String

list :: SyncFileStore -> IO [Name]
list fs = ifFound $ do
    let indexFilename = "index.json"
    contents <- retrieve fs (prefix </> indexFilename) Nothing
    return $ fromMaybe mempty (decode contents)

fetchData :: SyncFileStore -> Name -> IO (Maybe ByteString)
fetchData fs name = ifFound $
    retrieve fs (prefix </> name <.> "json") Nothing >>= return . Just

fetch :: SyncFileStore -> Name -> IO (Maybe Object)
fetch fs name = fetchData fs name >>= return . maybe Nothing decode

update :: SyncFileStore -> IO RevisionId
update = sync

ifFound :: Monoid a => IO a -> IO a
ifFound = handle (\NotFound -> return mempty)

prefix :: FilePath
prefix = "schema"
