module View.PatternLab exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (aubergine, oneDark)
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
        , div [ flexColumn, widthFill, spacingColumn15, oneDark ]
            [ introduction
            , theme
            , typography
            , textBlock
            , border
            , background
            , layout
            ]
        ]
    }


introduction : Html ()
introduction =
    section []
        [ h1 [ sizeImpact ]
            [ span [ serif, bold ] [ t "Zephyr:" ]
            , t " "
            , span [ sansSerif, italic ] [ t "Pattern" ]
            , t " "
            , span [ monospace, underline, bold ] [ t "Lab" ]
            ]
        , p [ padding10 ]
            [ t "... is a catalogue of Atomic Design in Zephyr app.\n"
            , t "I am bare texts in a paragraph, and I should be 12px in font-size (global default).\n"
            , t "By default this page has oneDark theme."
            ]
        , div [ padding10 ]
            [ h2 [ sizeHeadline ] [ t "Imports in code samples:" ]
            , pre [ padding10, Border.round5, Border.rect, Border.solid ]
                [ t """import Html exposing (..)
import Html.Attributes exposing (style)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (aubergine, oneDark)
import View.Atom.Typography exposing (..)"""
                ]
            ]
        ]


section : List (Attribute ()) -> List (Html ()) -> Html ()
section attrs =
    div ([ flexColumn, widthFill, spacingColumn15, padding10 ] ++ attrs)


theme : Html ()
theme =
    section []
        [ h1 [ sizeSection ] [ t "Theme" ]
        , withSource """p [ widthFill, padding15, oneDark ]
    [ t "This block has oneDark theme. Default background color and text color are automatically applied to its children.\\n"
    , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
    ]""" <|
            p [ widthFill, padding15, oneDark ]
                [ t "This block has oneDark theme. Default background color and text color are automatically applied to its children.\n"
                , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
                ]
        , withSource """p [ widthFill, padding15, aubergine ]
    [ t "This block has aubergine theme. Default background color and text color are automatically applied to its children.\\n"
    , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
    ]""" <|
            p [ widthFill, padding15, aubergine ]
                [ t "This block has aubergine theme. Default background color and text color are automatically applied to its children.\n"
                , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
                ]
        ]


typography : Html ()
typography =
    section []
        [ h1 [ sizeSection ] [ t "Typography" ]
        , fontFamilies
        , fontSizes
        , fontDecorations
        , fontColors
        ]


fontFamilies : Html ()
fontFamilies =
    section []
        [ h2 [ sizeTitle ] [ t "Font families" ]
        , withSource "p [ sansSerif ] [ t \"This paragraph uses a sans-serif font. あいうえお水兵リーベ\" ]" <|
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
    section []
        [ h2 [ sizeTitle ] [ t "Font sizes" ]
        , withSource "p [ sizeImpact ] [ t \"impact\" ]" <|
            p [ sizeImpact ] [ t "impact" ]
        , withSource "p [ sizeSection ] [ t \"section\" ]" <|
            p [ sizeSection ] [ t "section" ]
        , withSource "p [ sizeTitle ] [ t \"title\" ]" <|
            p [ sizeTitle ] [ t "title" ]
        , withSource "p [ sizeHeadline ] [ t \"headline\" ]" <|
            p [ sizeHeadline ] [ t "headline" ]
        , withSource "p [ sizeBase ] [ t \"base\" ]" <|
            p [ sizeBase ] [ t "base" ]
        , withSource "p [ sizeDetail ] [ t \"detail\" ]" <|
            p [ sizeDetail ] [ t "detail" ]
        ]


fontDecorations : Html ()
fontDecorations =
    section []
        [ h2 [ sizeTitle ] [ t "Font decorations" ]
        , withSource "p [] [ t \"This is normal text, あいうえお水兵リーベ\" ]" <|
            p [] [ t "This is normal text, あいうえお水兵リーベ" ]
        , withSource "p [ italic ] [ t \"This is italic text, あいうえお水兵リーベ\" ]" <|
            p [ italic ] [ t "This is italic text, あいうえお水兵リーベ" ]
        , withSource "p [ bold ] [ t \"This is bold text, あいうえお水兵リーベ\" ]" <|
            p [ bold ] [ t "This is bold text, あいうえお水兵リーベ" ]
        , withSource "p [ underline ] [ t \"This is underline text, あいうえお水兵リーベ\" ]" <|
            p [ underline ] [ t "This is underline text, あいうえお水兵リーベ" ]
        ]


fontColors : Html ()
fontColors =
    section []
        [ h2 [ sizeTitle ] [ t "Font colors" ]
        , section [ oneDark ]
            [ h3 [ sizeHeadline ] [ t "oneDark" ]
            , withSource """p [ colorText ] [ t "Text color, あいうえお水兵リーベ" ]""" <|
                p [ colorText ] [ t "Text color, あいうえお水兵リーベ" ]
            , withSource """p [ colorNote ] [ t "Note color, あいうえお水兵リーベ" ]""" <|
                p [ colorNote ] [ t "Note color, あいうえお水兵リーベ" ]
            , withSource """p [ colorLink ] [ t "Link color, あいうえお水兵リーベ", t " https://example.com" ]""" <|
                p [ colorLink ] [ t "Link color, あいうえお水兵リーベ", t " https://example.com" ]
            , withSource """p [ colorPrim ] [ t "Prim color, あいうえお水兵リーベ" ]""" <|
                p [ colorPrim ] [ t "Prim color, あいうえお水兵リーベ" ]
            , withSource """p [ colorSucc ] [ t "Succ color, あいうえお水兵リーベ" ]""" <|
                p [ colorSucc ] [ t "Succ color, あいうえお水兵リーベ" ]
            , withSource """p [ colorWarn ] [ t "Warn color, あいうえお水兵リーベ" ]""" <|
                p [ colorWarn ] [ t "Warn color, あいうえお水兵リーベ" ]
            , withSource """p [ colorErr ] [ t "Err color, あいうえお水兵リーベ" ]""" <|
                p [ colorErr ] [ t "Err color, あいうえお水兵リーベ" ]
            ]
        , section [ aubergine ]
            [ h3 [ sizeHeadline ] [ t "aubergine" ]
            , withSource """p [ colorText ] [ t "Text color, あいうえお水兵リーベ" ]""" <|
                p [ colorText ] [ t "Text color, あいうえお水兵リーベ" ]
            , withSource """p [ colorNote ] [ t "Note color, あいうえお水兵リーベ" ]""" <|
                p [ colorNote ] [ t "Note color, あいうえお水兵リーベ" ]
            , withSource """p [ colorLink ] [ t "Link color, あいうえお水兵リーベ", t " https://example.com" ]""" <|
                p [ colorLink ] [ t "Link color, あいうえお水兵リーベ", t " https://example.com" ]
            , withSource """p [ colorPrim ] [ t "Prim color, あいうえお水兵リーベ" ]""" <|
                p [ colorPrim ] [ t "Prim color, あいうえお水兵リーベ" ]
            , withSource """p [ colorSucc ] [ t "Succ color, あいうえお水兵リーベ" ]""" <|
                p [ colorSucc ] [ t "Succ color, あいうえお水兵リーベ" ]
            , withSource """p [ colorWarn ] [ t "Warn color, あいうえお水兵リーベ" ]""" <|
                p [ colorWarn ] [ t "Warn color, あいうえお水兵リーベ" ]
            , withSource """p [ colorErr ] [ t "Err color, あいうえお水兵リーベ" ]""" <|
                p [ colorErr ] [ t "Err color, あいうえお水兵リーベ" ]
            ]
        ]


textBlock : Html ()
textBlock =
    section []
        [ h1 [ sizeSection ] [ t "Text Blocks" ]
        , withSource """p [ Border.solid, Border.rect ]
    [ t "(Heading tags does not have attached styles. Use Typography classes/styles and make Molecules/Organisms.)\\n"
    , t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\\n"
    , t "and as you can see, respects literal line breaks.\\n"
    , t (String.repeat 2 lorem ++ "\\n")
    , t (String.repeat 10 iroha)
    ]""" <|
            p [ Border.solid, Border.rect ]
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


border : Html ()
border =
    section []
        [ h1 [ sizeSection ] [ t "Border" ]
        , withSource "div [ padding5, Border.solid, Border.rect ] [ t \"I'm surrounded by solid border.\" ]" <|
            div [ padding5, Border.solid, Border.rect ] [ t "I'm surrounded by solid border." ]
        , withSource "div [ padding5, Border.dotted, Border.rect ] [ t \"I'm surrounded by dotted border.\" ]" <|
            div [ padding5, Border.dotted, Border.rect ] [ t "I'm surrounded by dotted border." ]
        , withSource "div [ padding5, Border.dashed, Border.rect ] [ t \"I'm surrounded by dashed border.\" ]" <|
            div [ padding5, Border.dashed, Border.rect ] [ t "I'm surrounded by dashed border." ]
        , withSource "div [ padding5, Border.solid, Border.rect, Border.round2 ] [ t \"I'm surrounded by rounded solid border.\" ]" <|
            div [ padding5, Border.solid, Border.rect, Border.round2 ] [ t "I'm surrounded by rounded solid border." ]
        , withSource "div [ padding5, Border.solid, Border.rect, Border.round5 ] [ t \"I'm surrounded by more rounded solid border.\" ]" <|
            div [ padding5, Border.solid, Border.rect, Border.round5 ] [ t "I'm surrounded by more rounded solid border." ]
        ]


background : Html ()
background =
    section []
        [ h1 [ sizeSection ] [ t "Background" ]
        , section [ oneDark ]
            [ h2 [ sizeTitle ] [ t "oneDark" ]
            , withSource """div [ padding15 ] [ h3 [ sizeHeadline ] [ t "Default" ] ]""" <|
                div [ padding15 ] [ h3 [ sizeHeadline ] [ t "Default" ] ]
            , withSource """div [ padding15, Background.colorBg ] [ h3 [ sizeHeadline ] [ t "colorBg" ] ]""" <|
                div [ padding15, Background.colorBg ] [ h3 [ sizeHeadline ] [ t "colorBg" ] ]
            , withSource """div [ padding15, Background.colorMain ] [ h3 [ sizeHeadline ] [ t "colorMain" ] ]""" <|
                div [ padding15, Background.colorMain ] [ h3 [ sizeHeadline ] [ t "colorMain" ] ]
            , withSource """div [ padding15, Background.colorSub ] [ h3 [ sizeHeadline ] [ t "colorSub" ] ]""" <|
                div [ padding15, Background.colorSub ] [ h3 [ sizeHeadline ] [ t "colorSub" ] ]
            , withSource """div [ padding15, Background.colorNote ] [ h3 [ sizeHeadline ] [ t "colorNote" ] ]""" <|
                div [ padding15, Background.colorNote ] [ h3 [ sizeHeadline ] [ t "colorNote" ] ]
            , withSource """div [ padding15, Background.colorPrim ] [ h3 [ sizeHeadline ] [ t "colorPrim" ] ]""" <|
                div [ padding15, Background.colorPrim ] [ h3 [ sizeHeadline ] [ t "colorPrim" ] ]
            , withSource """div [ padding15, Background.colorSucc ] [ h3 [ sizeHeadline ] [ t "colorSucc" ] ]""" <|
                div [ padding15, Background.colorSucc ] [ h3 [ sizeHeadline ] [ t "colorSucc" ] ]
            , withSource """div [ padding15, Background.colorWarn ] [ h3 [ sizeHeadline ] [ t "colorWarn" ] ]""" <|
                div [ padding15, Background.colorWarn ] [ h3 [ sizeHeadline ] [ t "colorWarn" ] ]
            , withSource """div [ padding15, Background.colorErr ] [ h3 [ sizeHeadline ] [ t "colorErr" ] ]""" <|
                div [ padding15, Background.colorErr ] [ h3 [ sizeHeadline ] [ t "colorErr" ] ]
            ]
        , section [ aubergine ]
            [ h2 [ sizeTitle ] [ t "aubergine" ]
            , withSource """div [ padding15 ] [ h3 [ sizeHeadline ] [ t "Default" ] ]""" <|
                div [ padding15 ] [ h3 [ sizeHeadline ] [ t "Default" ] ]
            , withSource """div [ padding15, Background.colorBg ] [ h3 [ sizeHeadline ] [ t "colorBg" ] ]""" <|
                div [ padding15, Background.colorBg ] [ h3 [ sizeHeadline ] [ t "colorBg" ] ]
            , withSource """div [ padding15, Background.colorMain ] [ h3 [ sizeHeadline ] [ t "colorMain" ] ]""" <|
                div [ padding15, Background.colorMain ] [ h3 [ sizeHeadline ] [ t "colorMain" ] ]
            , withSource """div [ padding15, Background.colorSub ] [ h3 [ sizeHeadline ] [ t "colorSub" ] ]""" <|
                div [ padding15, Background.colorSub ] [ h3 [ sizeHeadline ] [ t "colorSub" ] ]
            , withSource """div [ padding15, Background.colorNote ] [ h3 [ sizeHeadline ] [ t "colorNote" ] ]""" <|
                div [ padding15, Background.colorNote ] [ h3 [ sizeHeadline ] [ t "colorNote" ] ]
            , withSource """div [ padding15, Background.colorPrim ] [ h3 [ sizeHeadline ] [ t "colorPrim" ] ]""" <|
                div [ padding15, Background.colorPrim ] [ h3 [ sizeHeadline ] [ t "colorPrim" ] ]
            , withSource """div [ padding15, Background.colorSucc ] [ h3 [ sizeHeadline ] [ t "colorSucc" ] ]""" <|
                div [ padding15, Background.colorSucc ] [ h3 [ sizeHeadline ] [ t "colorSucc" ] ]
            , withSource """div [ padding15, Background.colorWarn ] [ h3 [ sizeHeadline ] [ t "colorWarn" ] ]""" <|
                div [ padding15, Background.colorWarn ] [ h3 [ sizeHeadline ] [ t "colorWarn" ] ]
            , withSource """div [ padding15, Background.colorErr ] [ h3 [ sizeHeadline ] [ t "colorErr" ] ]""" <|
                div [ padding15, Background.colorErr ] [ h3 [ sizeHeadline ] [ t "colorErr" ] ]
            ]
        ]


layout : Html ()
layout =
    section []
        [ h1 [ sizeSection ] [ t "Layout" ]
        , h2 [ sizeTitle ] [ t "FlexBox and Width Control" ]
        , flexBox
        , h2 [ sizeTitle ] [ t "Padding" ]
        , padding
        , h2 [ sizeTitle ] [ t "Spacing" ]
        , spacing
        ]


flexBox : Html ()
flexBox =
    section []
        [ withSource """div [ widthFill, Border.solid, Border.rect ] [ t "I eat all available width. This is default behavior." ]""" <|
            div [ widthFill, Border.solid, Border.rect ] [ t "I eat all available width. This is default behavior." ]
        , withSource """div [ flexRow ]
    [ div [ Border.solid, Border.rect, Border.round5 ] [ t "I shrink as narrow as content length allows." ]
    , div [ Border.solid, Border.rect, Border.round5, style "flex-basis" "200px" ] [ t "I am fixed 200px width." ]
    , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "I grow." ]
    , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "I grow too." ]
    ]""" <|
            div [ flexRow ]
                [ div [ Border.solid, Border.rect, Border.round5 ] [ t "I shrink as narrow as content length allows." ]
                , div [ Border.solid, Border.rect, Border.round5, style "flex-basis" "200px" ] [ t "I am fixed 200px width." ]
                , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "We are children of flex row. I grow." ]
                , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "I grow too." ]
                ]
        , withSource """div [ flexColumn, style "height" "30vh" ]
    [ div [ Border.solid, Border.rect, Border.round5 ] [ t "I shrink as short as content length allows." ]
    , div [ Border.solid, Border.rect, Border.round5, style "flex-basis" "200px" ] [ t "I am fixed 200px height." ]
    , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "We are children of flex column. I grow." ]
    , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "I grow too." ]
    ]""" <|
            div [ flexColumn, style "height" "30vh" ]
                [ div [ Border.solid, Border.rect, Border.round5 ] [ t "I shrink as short as content length allows." ]
                , div [ Border.solid, Border.rect, Border.round5, style "flex-basis" "200px" ] [ t "I am fixed 200px height." ]
                , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "We are children of flex column. I grow." ]
                , div [ flexGrow, Border.solid, Border.rect, Border.round5 ] [ t "I grow too." ]
                ]
        ]


padding : Html ()
padding =
    section []
        [ withSource """div [ padding2, Border.solid, Border.rect ] [ t "I'm surrounded by 2px padding. ", t lorem ]""" <|
            div [ padding2, Border.solid, Border.rect ] [ t "I'm surrounded by 2px padding. ", t lorem ]
        , withSource """div [ padding5, Border.solid, Border.rect ] [ t "I'm surrounded by 5px padding. ", t lorem ]""" <|
            div [ padding5, Border.solid, Border.rect ] [ t "I'm surrounded by 5px padding. ", t lorem ]
        , withSource """div [ padding10, Border.solid, Border.rect ] [ t "I'm surrounded by 10px padding. ", t lorem ]""" <|
            div [ padding10, Border.solid, Border.rect ] [ t "I'm surrounded by 10px padding. ", t lorem ]
        , withSource """div [ padding15, Border.solid, Border.rect ] [ t "I'm surrounded by 15px padding. ", t lorem ]""" <|
            div [ padding15, Border.solid, Border.rect ] [ t "I'm surrounded by 15px padding. ", t lorem ]
        ]


spacing : Html ()
spacing =
    section []
        [ withSource """div [ flexRow, spacingRow5, Border.solid, Border.rect ]
    [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex row." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have left margins." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
    ]""" <|
            div [ flexRow, spacingRow5, Border.solid, Border.rect ]
                [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex row." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have left margins." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
                ]
        , withSource """div [ flexRow, spacingRow15, Border.solid, Border.rect ]
    [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex row." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have left margins." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
    ]""" <|
            div [ flexRow, spacingRow15, Border.solid, Border.rect ]
                [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex row." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have left margins." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
                ]
        , withSource """div [ flexColumn, spacingColumn5, Border.solid, Border.rect ]
    [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex column." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have top margins." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
    ]""" <|
            div [ flexColumn, spacingColumn5, Border.solid, Border.rect ]
                [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex column." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have top margins." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
                ]
        , withSource """div [ flexColumn, spacingColumn15, Border.solid, Border.rect ]
    [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex column." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have top margins." ]
    , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
    ]""" <|
            div [ flexColumn, spacingColumn15, Border.solid, Border.rect ]
                [ div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the first child of flex column." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the second one. Second and thereafter have top margins." ]
                , div [ flexGrow, Border.solid, Border.rect ] [ t "I'm the third one." ]
                ]
        ]
