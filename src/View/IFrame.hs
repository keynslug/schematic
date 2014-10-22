module View.IFrame (view) where

import Prelude hiding (div)
import Data.Monoid (mempty)

import Web.Scotty (ActionM)

import View.Snippets (blaze)
import View.Root (MenuList, renderLayout, renderNavBar, renderMenuList)
import Text.Blaze.Html5 (AttributeValue, (!), iframe, div, toValue)
import Text.Blaze.Html5.Attributes (width, height, src, class_)

view :: String -> MenuList -> ActionM ()
view url menu = blaze $ renderLayout "Schematic - IFrame" [] $ do
    renderNavBar (renderMenuList menu) mempty
    div ! class_ "container" $
        iframe ! width "100%" ! height "540" ! src (toValue url) $ mempty
