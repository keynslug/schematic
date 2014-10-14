module Data.Schemata (
      Name
    , list
    , actualRevision
    , bakeData
    , rawSchema
    , getSchema
    , rawDataObject
    , getDataObject
    , putDataObject
    , update
    ) where

import Control.Applicative (pure, liftA2)
import Control.Exception

import Data.Monoid
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (pack)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (ByteString)
import Data.FileStore.GitSync

import System.FilePath ((</>), (<.>))

type Name = String

data Type = Schema | DataObject

list :: SyncFileStore -> IO [Name]
list fs = ifFound $ do
    contents <- retrieve fs (filePath Schema "index") Nothing
    return $ fromMaybe mempty (decode contents)

actualRevision :: SyncFileStore -> IO RevisionId
actualRevision = flip latest ""

bakeData :: SyncFileStore -> IO Object
bakeData repo = do
    index <- list repo
    objects <- mapM (getDataObject repo) index
    let pairs = catMaybes $ zipWith (liftA2 (.=) . pure) (map pack index) objects
    let Object result = object pairs
    return result

raw_ :: Type -> SyncFileStore -> Name -> IO (Maybe ByteString)
raw_ tp fs name = ifFound $
    retrieve fs (filePath tp name) Nothing >>= return . Just

get_ :: Type -> SyncFileStore -> Name -> IO (Maybe Object)
get_ tp fs name = return . maybe Nothing decode =<< raw_ tp fs name

rawSchema :: SyncFileStore -> Name -> IO (Maybe ByteString)
rawSchema = raw_ Schema

getSchema :: SyncFileStore -> Name -> IO (Maybe Object)
getSchema = get_ Schema

rawDataObject :: SyncFileStore -> Name -> IO (Maybe ByteString)
rawDataObject = raw_ DataObject

getDataObject :: SyncFileStore -> Name -> IO (Maybe Object)
getDataObject = get_ DataObject

putDataObject :: SyncFileStore -> Name -> Object -> IO RevisionId
putDataObject fs name o = do
    let author = Author "Schematic" "schematic@keynfawk.es"
    let description = "Changes to '" <> name <> "' data object"
    let fp = (filePath DataObject name)
    save fs fp author description (encodePretty o)
    latest fs fp

update :: SyncFileStore -> IO RevisionId
update = sync

ifFound :: Monoid a => IO a -> IO a
ifFound = handle (\NotFound -> return mempty)

filePath :: Type -> Name -> FilePath
filePath tp name = prefix tp </> name <.> "json"

prefix :: Type -> FilePath
prefix Schema = "schema"
prefix DataObject = "data"
