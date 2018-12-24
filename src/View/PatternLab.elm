module View.PatternLab exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes
import View.Atom.Typography exposing (..)
import View.Stylesheet


main : Program () () ()
main =
    Browser.document
        { init = always ( (), Cmd.none )
        , view = always view
        , update = \() () -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        }


view : { title : String, body : List (Html.Html ()) }
view =
    { title = "Zephyr: Pattern Lab"
    , body =
        [ View.Stylesheet.render
        , atomsView
        ]
    }


atomsView : Html.Html ()
atomsView =
    Html.div []
        [ h1 [ titleFontSize ] [ t "[h1.titleFontSize] Pattern Lab" ]
        , p []
            [ t "[p] ... is a catalogue of UI Atoms/Molecules in Zephyr app. "
            , t "These are bare texts in a paragraph, and it should be 12px in font-size."
            ]
        , h1 [ titleFontSize ] [ t "[h1.titleFontSize] Here's available font-families" ]
        , p [ sansSerif ] [ t "This paragraph uses a sans-serif font. あいうえお水兵リーベ" ]
        , p [ serif ] [ t "[p.serif] This paragraph uses a serif font. あいうえお水兵リーベ" ]
        , p [ monospace ] [ t "[p.monospace] This paragraph uses a monospace font. あいうえお水兵リーベ" ]
        , h1 [ titleFontSize ] [ t "[h1.titleFontSize] Font size comparisons" ]
        , p [ baseFontSize ] [ t "[p.baseFontSize] baseFontSize" ]
        , p [ detailFontSize ] [ t "[p.detailFontSize] detailFontSize" ]
        , p [ headlineFontSize ] [ t "[p.headlineFontSize] headlineFontSize" ]
        , p [ titleFontSize ] [ t "[p.titleFontSize] titleFontSize" ]
        , p [ impactFontSize ] [ t "[p.impactFontSize] impactFontSize" ]
        ]
