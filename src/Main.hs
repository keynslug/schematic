import Web.Scotty

import Control.Monad.IO.Class (liftIO)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import Data.FileStore.GitSync (fileStore)

import Settings
import Schemata

import View.Snippets
import qualified View.Root

main :: IO ()
main = do

    s <- readSettings "config.yaml"
    let repo = fileStore (repoPath s)

    scotty (port s) $ do

        middleware $ logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" View.Root.view

        get "/schema/index" $ do
            index <- liftIO $ list repo
            json index

        get "/schema/:name" $ do
            name <- param "name"
            contents <- liftIO $ fetchData repo name
            addHeader "Content-Type" "application/json; charset=utf-8"
            maybeNotFound raw contents
