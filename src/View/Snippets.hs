module View.Snippets (
    shims,
    injectScript,
    injectStylesheet,
    embedStylesheet,
    blaze
    ) where

import Web.Scotty                    (ActionM, html)

import Data.Monoid                   (mempty)
import Data.Text.Lazy                (Text, toStrict)

import Text.Blaze.Internal           (preEscapedText)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              (Html, AttributeValue, meta, script, link, style, (!))
import Text.Blaze.Html5.Attributes   (charset, content, href, name, rel, src, type_, media, httpEquiv)


shims :: Html
shims = do
    preEscapedText "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
    preEscapedText "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
    preEscapedText "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
    preEscapedText "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width"

injectScript :: AttributeValue -> Html
injectScript url = script ! src url $ mempty

injectStylesheet :: AttributeValue -> Html
injectStylesheet url = link ! href url ! rel "stylesheet" ! media "screen"

embedStylesheet :: Text -> Html
embedStylesheet text = style ! type_ "text/css" $ preEscapedText $ toStrict text

blaze :: Html -> ActionM ()
blaze = html . renderHtml
