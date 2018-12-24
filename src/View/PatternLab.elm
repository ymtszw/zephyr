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
        , introduction
        , typography
        , textBlock
        ]
    }


introduction : Html ()
introduction =
    div []
        [ h1 [ impactFontSize ] [ t "Pattern Lab" ]
        , p []
            [ t "... is a catalogue of Atomic Design in Zephyr app.\n"
            , t "I am bare texts in a paragraph, and I should be 12px in font-size (global default).\n"
            ]
        ]


typography : Html ()
typography =
    div []
        [ h1 [ titleFontSize ] [ t "Typography" ]
        , h2 [ headlineFontSize ] [ t "Font families" ]
        , fontFamilies
        , h2 [ headlineFontSize ] [ t "Font sizes" ]
        , fontSizes
        ]


fontFamilies : Html.Html ()
fontFamilies =
    div []
        [ p [ sansSerif ] [ t "This paragraph uses a sans-serif font. あいうえお水兵リーベ" ]
        , p [ serif ] [ t "[p.serif] This paragraph uses a serif font. あいうえお水兵リーベ" ]
        , p [ monospace ] [ t "[p.monospace] This paragraph uses a monospace font. あいうえお水兵リーベ" ]
        ]


fontSizes : Html.Html ()
fontSizes =
    div []
        [ p [ baseFontSize ] [ t "baseFontSize" ]
        , p [ detailFontSize ] [ t "detailFontSize" ]
        , p [ headlineFontSize ] [ t "headlineFontSize" ]
        , p [ titleFontSize ] [ t "titleFontSize" ]
        , p [ impactFontSize ] [ t "impactFontSize" ]
        ]


textBlock : Html.Html ()
textBlock =
    div []
        [ h1 [ titleFontSize ] [ t "Text Blocks" ]
        , p []
            [ t "(Heading tags does not have attached styles. Use Typography classes/styles and make Molecules/Organisms.)\n"
            , t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\n"
            , t "and as you can see, respects literal line breaks.\n"
            ]
        , pre [] [ t "[pre] in pre tag, texts have monospace font." ]
        ]
