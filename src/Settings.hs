module Settings where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Data.Yaml

data Settings = Settings {
    port :: Int,
    repoPath :: FilePath,
    iframeTarget :: String
    } deriving (Show, Read, Eq)

readSettings :: FilePath -> IO Settings
readSettings fp =
    decodeFileEither fp >>= either (fail "Wrong settings") parse where
        parse = parseMonad parser
        parser (Object o) = Settings
            <$> o .:? "port" .!= 8000
            <*> o .: "repositoryPath"
            <*> o .: "iframeTarget"
        parser _ = mzero
