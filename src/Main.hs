import Web.Scotty

import Control.Monad.IO.Class (liftIO)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import Data.Monoid (mempty)
import qualified Data.List as List (delete)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as LT (pack)
import Data.FileStore.GitSync (fileStore)
import Data.Schemata

import Settings

import View.Snippets
import View.Root (MenuList(..))
import qualified View.Root

main :: IO ()
main = do

    s <- readSettings "config.yaml"
    let repo = fileStore (repoPath s)

    scotty (port s) $ do

        middleware $ logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ do
            index <- liftIO $ list repo
            redirect $ LT.pack $ routeData (head index)

        get "/edit/:name" $ do
            sname <- param "name"
            index <- liftIO $ list repo
            let name = T.pack sname
            View.Root.view name $ makeMenu name $ List.delete sname index

        get "/schema/:name" $ do
            name <- param "name"
            contents <- liftIO $ rawSchema repo name
            addHeader "Content-Type" "application/json; charset=utf-8"
            maybeNotFound raw contents

        get "/schemata" $ do
            index <- liftIO $ list repo
            json index

        post "/schemata/update" $ do
            _ <- liftIO $ update repo
            callback <- header "Referer"
            case callback of
                (Just url) -> redirect url
                _          -> text mempty

        get "/data/revision" $ do
            json =<< (liftIO $ actualRevision repo)

        get "/data/:name" $ do
            name <- param "name"
            contents <- liftIO $ getDataObject repo name
            maybeNotFound json contents

        post "/data/:name" $ do
            name <- param "name"
            contents <- jsonData
            revision <- liftIO $ putDataObject repo name contents
            json revision

            where
                makeMenu sel es = MenuList sel $ flip map es $ \e -> (T.pack e, T.pack $ routeData e)
                routeData e = "/edit/" ++ e
