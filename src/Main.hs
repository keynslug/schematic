import Web.Scotty

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import qualified View.Root

main = scotty 8000 $ do

    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" View.Root.view
