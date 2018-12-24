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
        [ h1 [ impactFontSize ]
            [ span [ serif, bold ] [ t "Zephyr:" ]
            , t " "
            , span [ sansSerif, italic ] [ t "Pattern" ]
            , t " "
            , span [ monospace, underline, bold ] [ t "Lab" ]
            ]
        , p []
            [ t "... is a catalogue of Atomic Design in Zephyr app.\n"
            , t "I am bare texts in a paragraph, and I should be 12px in font-size (global default).\n"
            ]
        ]


typography : Html ()
typography =
    div []
        [ h1 [ sectionFontSize ] [ t "Typography" ]
        , h2 [ titleFontSize ] [ t "Font families" ]
        , fontFamilies
        , h2 [ titleFontSize ] [ t "Font sizes" ]
        , fontSizes
        , h2 [ titleFontSize ] [ t "Font decorations" ]
        , fontDecorations
        ]


fontFamilies : Html.Html ()
fontFamilies =
    div []
        [ p [ sansSerif ] [ t "This paragraph uses a sans-serif font. あいうえお水兵リーベ" ]
        , p [ serif ] [ t "This paragraph uses a serif font. あいうえお水兵リーベ" ]
        , p [ monospace ] [ t "This paragraph uses a monospace font. あいうえお水兵リーベ" ]
        ]


fontSizes : Html.Html ()
fontSizes =
    div []
        [ p [ impactFontSize ] [ t "impactFontSize" ]
        , p [ sectionFontSize ] [ t "sectionFontSize" ]
        , p [ titleFontSize ] [ t "titleFontSize" ]
        , p [ headlineFontSize ] [ t "headlineFontSize" ]
        , p [ baseFontSize ] [ t "baseFontSize" ]
        , p [ detailFontSize ] [ t "detailFontSize" ]
        ]


fontDecorations : Html.Html ()
fontDecorations =
    div []
        [ p [] [ t "This is normal text, あいうえお水兵リーベ" ]
        , p [ italic ] [ t "This is italic text, あいうえお水兵リーベ" ]
        , p [ bold ] [ t "This is bold text, あいうえお水兵リーベ" ]
        , p [ underline ] [ t "This is underline text, あいうえお水兵リーベ" ]
        ]


textBlock : Html.Html ()
textBlock =
    div []
        [ h1 [ sectionFontSize ] [ t "Text Blocks" ]
        , p []
            [ t "(Heading tags does not have attached styles. Use Typography classes/styles and make Molecules/Organisms.)\n"
            , t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\n"
            , t "and as you can see, respects literal line breaks.\n"
            ]
        , pre [] [ t "In pre tag, texts have monospace font." ]
        ]
