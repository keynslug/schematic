module Style.Common (styles) where

import Prelude hiding ((**), div)

import Clay
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)

styles :: Text
styles = render $ do
    ul # ".nav" # ".navbar-nav" |> (a <> li |> a) ? do
        color white
        hover & do background $ rgba 0 0 0 20
        focus & do background $ rgba 0 0 0 20
    nav # ".navbar" ? do
        color white
        border solid 0 white
        background validGreen
        "#errors" ? do
            visibility hidden
        ".invalid" & do
            background alertRed
            "#errors" ? do
                visibility visible
        ".navbar-brand" ? do
            fontSize $ em 1.6

    div # "#editor" ? do
        h3 ? do
            fontSize $ px 20
            marginTop $ px 10
        button # ".btn" ? do
            fontSize (px 12)
            lineHeight (em 1.5)
            sym2 padding (px 1) (px 5)
            sym borderRadius nil
        div # ".row" ? do
            paddingLeft $ px 9
        div # ".well" ? do
            sym borderRadius nil
        ".form-control" ? do
            height (px 24)
            sym2 padding (px 2) (px 6)
            sym borderRadius nil

        colorizeLevels $ [lightpink, lightgreen, lightblue, violet, salmon]

colorizeLevels :: [Color] -> Css
colorizeLevels [] = return ()
colorizeLevels (nextColor : rest) =
    div # ".well" ? do
        borderLeft solid (px 4) nextColor
        colorizeLevels rest

validGreen :: Color
validGreen = "#00CC69"

alertRed :: Color
alertRed = "#CC0300"
