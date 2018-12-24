module View.PatternLab exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes
import View.Atom.Layout exposing (..)
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


view : { title : String, body : List (Html ()) }
view =
    { title = "Zephyr: Pattern Lab"
    , body =
        [ View.Stylesheet.render
        , div [ flexColumn, widthFill, spacingColumn15 ]
            [ introduction
            , typography
            , textBlock
            ]
        ]
    }


introduction : Html ()
introduction =
    div [ flexColumn, widthFill, spacingColumn15 ]
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
    div [ flexColumn, widthFill, spacingColumn15 ]
        [ h1 [ sectionFontSize ] [ t "Typography" ]
        , h2 [ titleFontSize ] [ t "Font families" ]
        , fontFamilies
        , h2 [ titleFontSize ] [ t "Font sizes" ]
        , fontSizes
        , h2 [ titleFontSize ] [ t "Font decorations" ]
        , fontDecorations
        ]


fontFamilies : Html ()
fontFamilies =
    div [ flexColumn, widthFill, spacingColumn15 ]
        [ withSource "p [ sansSerif ] [ t \"This paragraph uses a sans-serif font. あいうえお水兵リーベ\" ]" <|
            p [ sansSerif ] [ t "This paragraph uses a sans-serif font. あいうえお水兵リーベ" ]
        , withSource "p [ serif ] [ t \"This paragraph uses a serif font. あいうえお水兵リーベ\" ]" <|
            p [ serif ] [ t "This paragraph uses a serif font. あいうえお水兵リーベ" ]
        , withSource "p [ monospace ] [ t \"This paragraph uses a monospace font. あいうえお水兵リーベ\" ]" <|
            p [ monospace ] [ t "This paragraph uses a monospace font. あいうえお水兵リーベ" ]
        ]


withSource : String -> Html () -> Html ()
withSource source toRender =
    div [ flexRow, flexCenter, widthFill, spacingRow15 ]
        [ div [ flexGrow ] [ toRender ]
        , pre [ flexGrow ] [ t source ]
        ]


fontSizes : Html ()
fontSizes =
    div [ flexColumn, widthFill, spacingColumn15 ]
        [ withSource "p [ impactFontSize ] [ t \"impact\" ]" <|
            p [ impactFontSize ] [ t "impact" ]
        , withSource "p [ sectionFontSize ] [ t \"section\" ]" <|
            p [ sectionFontSize ] [ t "section" ]
        , withSource "p [ titleFontSize ] [ t \"title\" ]" <|
            p [ titleFontSize ] [ t "title" ]
        , withSource "p [ headlineFontSize ] [ t \"headline\" ]" <|
            p [ headlineFontSize ] [ t "headline" ]
        , withSource "p [ baseFontSize ] [ t \"base\" ]" <|
            p [ baseFontSize ] [ t "base" ]
        , withSource "p [ detailFontSize ] [ t \"detail\" ]" <|
            p [ detailFontSize ] [ t "detail" ]
        ]


fontDecorations : Html ()
fontDecorations =
    div [ flexColumn, widthFill, spacingColumn15 ]
        [ withSource "p [] [ t \"This is normal text, あいうえお水兵リーベ\" ]" <|
            p [] [ t "This is normal text, あいうえお水兵リーベ" ]
        , withSource "p [ italic ] [ t \"This is italic text, あいうえお水兵リーベ\" ]" <|
            p [ italic ] [ t "This is italic text, あいうえお水兵リーベ" ]
        , withSource "p [ bold ] [ t \"This is bold text, あいうえお水兵リーベ\" ]" <|
            p [ bold ] [ t "This is bold text, あいうえお水兵リーベ" ]
        , withSource "p [ underline ] [ t \"This is underline text, あいうえお水兵リーベ\" ]" <|
            p [ underline ] [ t "This is underline text, あいうえお水兵リーベ" ]
        ]


textBlock : Html ()
textBlock =
    div [ flexColumn, widthFill, spacingColumn15 ]
        [ h1 [ sectionFontSize ] [ t "Text Blocks" ]
        , withSource """p []
    [ t "(Heading tags does not have attached styles. Use Typography classes/styles and make Molecules/Organisms.)\\n"
    , t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\\n"
    , t "and as you can see, respects literal line breaks.\\n"
    , t (String.repeat 2 lorem ++ "\\n")
    , t (String.repeat 10 iroha)
    ]""" <|
            p []
                [ t "(Heading tags does not have attached styles. Use Typography classes/styles and make Molecules/Organisms.)\n"
                , t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\n"
                , t "and as you can see, respects literal line breaks.\n"
                , t (String.repeat 2 lorem ++ "\n")
                , t (String.repeat 10 iroha)
                ]
        , withSource "pre [] [ t \"In pre tag, texts have monospace font.\" ]" <|
            pre [] [ t "In pre tag, texts have monospace font." ]
        ]


lorem : String
lorem =
    """Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."""


iroha : String
iroha =
    "いろはにほへと散りぬるをわかよ誰そ常ならむ有為の奥山今日越えてあさきゆめみしゑひもせすん"
