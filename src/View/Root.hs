module View.Root (view) where

import Prelude                       hiding (div, head, id, span)

import View.Snippets
import Style.Common                  (styles)

import Web.Scotty                    (ActionM)

import Data.Monoid                   (mempty)

import Text.Blaze.Html5              (Html, a, body, div, nav, docTypeHtml, head, li, title, ul, (!))
import Text.Blaze.Html5.Attributes   (class_, href, id)

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
    head $ do
        shims
        title t
        injectStylesheet "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
        embedStylesheet styles
    body $ do
        navBar >> b
        injectScript "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
        injectScript "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
        injectScript "/js/jsoneditor.js"
        injectScript "/js/root.js"

navBar :: Html
navBar = nav ! class_ "navbar navbar-static-top valid" $ div ! class_ "container" $ do
    ul ! class_ "nav navbar-nav" $
        li ! class_ "navbar-brand" $ "Schematic"
    ul ! class_ "nav navbar-nav navbar-right" $
        li $ a ! id "errors" ! href "#" $ "Errors"

view :: ActionM ()
view = blaze $ layout "Schematic" $ do
    div ! class_ "container" $ do
        div ! id "editor" $ mempty
