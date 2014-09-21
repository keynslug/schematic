module View.Root (view) where

import Prelude                       hiding (div, head, id, span)

import View.Snippets
import qualified Style.Common

import Web.Scotty                    (ActionM)

import Data.Monoid                   (mempty)

import Text.Blaze.Html5 (
    Html, a, body, button, dataAttribute, div, nav, docTypeHtml,
    form, h1, h2, head, input, li, link, p, title, ul, (!)
    )

import Text.Blaze.Html5.Attributes (
    class_, href, id, name, placeholder, src, type_
    )


layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
    head $ do
        shims
        title t
        injectStylesheet "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
        embedStylesheet Style.Common.styles
    body $ do
        navBar >> b
        injectScript "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
        injectScript "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
        injectScript "/js/jsoneditor.js"
        injectScript "/js/root.js"

navBar :: Html
navBar = nav ! cls "navbar navbar-static-top valid" $ div ! cls "container" $ do
    ul ! cls "nav navbar-nav" $
        li ! cls "navbar-brand" $ "Schematic"
    ul ! cls "nav navbar-nav navbar-right" $
        li $ a ! id "errors" ! href "#" $ "Errors"

cls = class_
attr = dataAttribute


view :: ActionM ()
view = blaze $ layout "Schematic" $ do
    div ! class_ "container" $ do
        div ! id "editor" $ mempty
