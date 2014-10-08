module View.Root (MenuList(..), view) where

import Prelude                       hiding (div, head, id, span)

import View.Snippets
import Style.Common                  (styles)

import Web.Scotty                    (ActionM)

import Control.Monad                 (forM_)
import Data.Monoid                   (mempty)
import Data.Text                     (Text)

import Text.Blaze.Html5              ( Html
                                     , a, body, div, nav, docTypeHtml, head, li, title, ul, (!)
                                     , dataAttribute, customAttribute, span, toHtml, toValue
                                     )
import Text.Blaze.Html5.Attributes   (class_, href, id)

type MenuEntry = (Text, Text)
data MenuList = MenuList Text [MenuEntry]

renderLayout :: Html -> Html -> Html
renderLayout t b = docTypeHtml $ do
    head $ do
        shims
        title t
        injectStylesheet "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
        embedStylesheet styles
    body $ do
        b
        injectScript "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
        injectScript "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
        injectScript "/js/jsoneditor.js"
        injectScript "/js/root.js"

renderNavBar :: Html -> Html -> Html
renderNavBar menuBlock rightBlock = nav ! class_ "navbar navbar-static-top valid" $ div ! class_ "container" $ do
    ul ! class_ "nav navbar-nav" $ do
        a ! href "/" $ li ! class_ "navbar-brand" $ "Schematic"
        menuBlock
    ul ! class_ "nav navbar-nav navbar-right" $
        rightBlock

renderMenuList :: MenuList -> Html
renderMenuList (MenuList sel es) =
    li ! class_ "dropdown" $ do
        a ! href "#" ! class_ "dropdown-toggle" ! dataAttribute "toggle" "dropdown" $ do
            toHtml sel
            span ! class_ "caret" $ mempty
        ul ! class_ "dropdown-menu" ! customAttribute "role" "menu" $
            forM_ es $ \(content, ref) -> li $ a ! href (toValue ref) $ toHtml content

renderRightButtons :: Html
renderRightButtons =
    li $ a ! id "errors" ! href "#" $ "Errors"

view :: Text -> MenuList -> ActionM ()
view name menu = blaze $ renderLayout "Schematic" $ do
    renderNavBar (renderMenuList menu) renderRightButtons
    div ! class_ "container" $ do
        div ! id "editor" ! dataAttribute "name" (toValue name) $ mempty
