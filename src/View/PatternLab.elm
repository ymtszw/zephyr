module View.PatternLab exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StringExtra
import Url exposing (Url)
import Url.Builder
import Url.Parser as U
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Button as Button
import View.Atom.Layout exposing (..)
import View.Atom.TextBlock exposing (forceBreak)
import View.Atom.Theme exposing (aubergine, oneDark, oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Stylesheet


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = Arrived
        }


type alias Model =
    { key : Key
    , route : Route
    }


type Route
    = Top
    | Typography
    | TextBlock
    | Border
    | Background
    | Layout
    | Button


routeToString : Route -> String
routeToString r =
    let
        abs_ path =
            Url.Builder.absolute path []
    in
    case r of
        Top ->
            abs_ []

        Typography ->
            abs_ [ "typography" ]

        TextBlock ->
            abs_ [ "text_block" ]

        Border ->
            abs_ [ "border" ]

        Background ->
            abs_ [ "background" ]

        Layout ->
            abs_ [ "layout" ]

        Button ->
            abs_ [ "button" ]


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key, route = urlToRoute url }, Cmd.none )


urlToRoute : Url -> Route
urlToRoute url =
    let
        urlParser =
            U.oneOf
                [ U.map Typography (U.s "typography")
                , U.map TextBlock (U.s "text_block")
                , U.map Border (U.s "border")
                , U.map Background (U.s "background")
                , U.map Layout (U.s "layout")
                , U.map Button (U.s "button")
                ]
    in
    Maybe.withDefault Top (U.parse urlParser url)


type Msg
    = NoOp
    | GoTo Route
    | Arrived Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        GoTo route ->
            ( m, Browser.Navigation.pushUrl m.key (routeToString route) )

        Arrived url ->
            ( { m | route = urlToRoute url }, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view m =
    { title = "Zephyr: Pattern Lab"
    , body =
        [ View.Stylesheet.render
        , div [ flexColumn, widthFill, spacingColumn15, oneDark ] <|
            (::) (navi m.route) <|
                case m.route of
                    Top ->
                        [ introduction, theme ]

                    Typography ->
                        [ typography ]

                    TextBlock ->
                        [ textBlock ]

                    Border ->
                        [ border ]

                    Background ->
                        [ background ]

                    Layout ->
                        [ layout ]

                    Button ->
                        [ button_ ]
        ]
    }


navi : Route -> Html Msg
navi r =
    div [ flexColumn, flexCenter, spacingColumn10, padding15 ]
        [ div [ flexRow, flexCenter, spacingRow15 ]
            [ h2 [ sizeHeadline, bold ] [ t "Atoms" ]
            , naviButton r Top "Top"
            , naviButton r Typography "Typography"
            , naviButton r TextBlock "TextBlock"
            , naviButton r Border "Border"
            , naviButton r Background "Background"
            , naviButton r Layout "Layout"
            , naviButton r Button "Button"
            ]
        ]


naviButton : Route -> Route -> String -> Html Msg
naviButton current hit btnLabel =
    let
        c =
            if current == hit then
                Button.succ

            else
                Button.prim
    in
    button [ sizeHeadline, padding10, onClick (GoTo hit), c ] [ t btnLabel ]


introduction : Html Msg
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
            [ t "... is a catalogue of Atomic Design in Zephyr app. "
            , t "I am bare texts in a paragraph, and I should be 12px in font-size (global default). "
            , t "By default this page has oneDark theme."
            ]
        , div [ padding10 ]
            [ h2 [ sizeHeadline ]
                [ t "Stylesheet length: "
                , t (StringExtra.punctuateNumber View.Stylesheet.length)
                ]
            ]
        , div [ padding10 ]
            [ h2 [ sizeHeadline ] [ t "Imports in code samples:" ]
            , pre [ padding10, Border.round5, Border.w1, Border.solid ]
                [ t """import Html exposing (..)
import Html.Attributes exposing (style)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Button as Button
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (aubergine, oneDark)
import View.Atom.Typography exposing (..)"""
                ]
            ]
        ]


section : List (Attribute Msg) -> List (Html Msg) -> Html Msg
section attrs =
    div ([ flexColumn, widthFill, flexCenter, spacingColumn15, padding10 ] ++ attrs)


theme : Html Msg
theme =
    section []
        [ h1 [ sizeSection ] [ t "Theme" ]
        , withSource """div [ padding15, oneDark ]
    [ t "This block has oneDark theme. Default background color and text color are automatically applied to its children. "
    , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
    ]""" <|
            div [ padding15, oneDark ]
                [ t "This block has oneDark theme. Default background color and text color are automatically applied to its children. "
                , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
                ]
        , withSource """div [ padding15, aubergine ]
    [ t "This block has aubergine theme. Default background color and text color are automatically applied to its children. "
    , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
    ]""" <|
            div [ padding15, aubergine ]
                [ t "This block has aubergine theme. Default background color and text color are automatically applied to its children. "
                , t "Child elements may apply specific color mode classes for decorations. See Typography/Border/Background sections."
                ]
        ]


typography : Html Msg
typography =
    section []
        [ h1 [ sizeSection ] [ t "Typography" ]
        , fontFamilies
        , fontSizes
        , fontDecorations
        , fontColors
        ]


fontFamilies : Html Msg
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


withSource : String -> Html Msg -> Html Msg
withSource source toRender =
    div [ growRow, flexCenter, widthFill, spacingRow15 ]
        [ div [] [ toRender ]
        , pre [] [ t source ]
        ]


fontSizes : Html Msg
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


fontDecorations : Html Msg
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


fontColors : Html Msg
fontColors =
    let
        coloredTexts theme_ themeText =
            section [ theme_ ]
                [ h3 [ sizeHeadline ] [ t themeText ]
                , withSource """p [ colorText ] [ t "Text color, あいうえお水兵リーベ" ]""" <|
                    p [ colorText ] [ t "Text color, あいうえお水兵リーベ" ]
                , withSource """p [ colorNote ] [ t "Note color, あいうえお水兵リーベ" ]""" <|
                    p [ colorNote ] [ t "Note color, あいうえお水兵リーベ" ]
                , withSource """p [ colorLink ] [ t "Link color, あいうえお水兵リーベ" ]""" <|
                    p [ colorLink ] [ t "Link color, あいうえお水兵リーベ" ]
                , withSource """p [ colorPrim ] [ t "Prim color, あいうえお水兵リーベ" ]""" <|
                    p [ colorPrim ] [ t "Prim color, あいうえお水兵リーベ" ]
                , withSource """p [ colorSucc ] [ t "Succ color, あいうえお水兵リーベ" ]""" <|
                    p [ colorSucc ] [ t "Succ color, あいうえお水兵リーベ" ]
                , withSource """p [ colorWarn ] [ t "Warn color, あいうえお水兵リーベ" ]""" <|
                    p [ colorWarn ] [ t "Warn color, あいうえお水兵リーベ" ]
                , withSource """p [ colorErr ] [ t "Err color, あいうえお水兵リーベ" ]""" <|
                    p [ colorErr ] [ t "Err color, あいうえお水兵リーベ" ]
                , withSource """p [] [ t "With ", code [] [ t "Inline Code" ], t " 水兵リーベ" ]""" <|
                    p [] [ t "With ", code [] [ t "Inline Code" ], t " 水兵リーベ" ]
                , withSource """p [] [ code [] [ t "More inline code with varying texts. ygqjp0123456789" ], code [] [ t (String.repeat 2 iroha) ] ]""" <|
                    p [] [ code [] [ t "More inline code with varying texts. ygqjp0123456789" ], code [] [ t (String.repeat 2 iroha) ] ]
                , withSource """p [] [ t "Auto style on links: ", link [] { url = "https://example.com", children = [ t "example.com" ] } ]""" <|
                    p [] [ t "Auto style on links: ", link [] { url = "https://example.com", children = [ t "example.com" ] } ]
                ]
    in
    section []
        [ h2 [ sizeTitle ] [ t "Font colors" ]
        , coloredTexts oneDark "oneDark"
        , coloredTexts aubergine "aubergine"
        ]


textBlock : Html Msg
textBlock =
    section []
        [ h1 [ sizeSection ] [ t "Text Blocks" ]
        , withSource """h2 [] [ t "Heading tags does not have default styles. Use Typography classes/styles." ]""" <|
            h2 [] [ t "Heading tags does not have default styles. Use Typography classes/styles." ]
        , withSource """p []
    [ t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\\n"
    , t "and as you can see, do not respect literal line breaks."
    ]""" <|
            p []
                [ t "By default, all text blocks (p,pre,h1-h6) have line-height of 1.3em,\n"
                , t "and as you can see, respects literal line breaks."
                ]
        , withSource """p [] [ t (String.repeat 2 lorem) ]""" <| p [] [ t (String.repeat 2 lorem) ]
        , withSource """p [] [ t (String.repeat 10 iroha) ]""" <| p [] [ t (String.repeat 10 iroha) ]
        , withSource """p []
    [ t "Significantly long alphanumeric strings can overflow from its parent blocks, or stretch its container if it is a flex item. Like this: "
    , t (String.repeat 20 "abcd0123")
    ]""" <|
            p []
                [ t "Significantly long alphanumeric strings can overflow from its parent blocks, or stretch its container if it is a flex item. Like this: "
                , t (String.repeat 20 "abcd0123")
                ]
        , withSource """p [ forceBreak ]
    [ t "With "
    , code [] [ t "forceBreak" ]
    , t " attribute, literal line breaks are respected\\n"
    , t "like this!\\n"
    , t "With "
    , code [] [ t "forceBreak" ]
    , t ", even significantly long alphanumeric strings are contained within their parent blocks like so:\\n"
    , t (String.repeat 100 "abcd0123")
    ]""" <|
            p [ forceBreak ]
                [ t "With "
                , code [] [ t "forceBreak" ]
                , t " attribute, literal line breaks are respected\n"
                , t "like this!\n"
                , t "With "
                , code [] [ t "forceBreak" ]
                , t ", even significantly long alphanumeric strings are contained within their parent blocks like so:\n"
                , t (String.repeat 100 "abcd0123")
                ]
        , withSource """pre []
    [ t "In <pre>, texts have monospace font.\\n"
    , t "Also, by default it breaks texts in the same manner as forceBreak.\\n"
    , t (String.repeat 100 "abcd0123")
    ]""" <|
            pre []
                [ t "In <pre>, texts have monospace font.\n"
                , t "Also, by default it breaks texts in the same manner as forceBreak.\n"
                , t (String.repeat 100 "abcd0123")
                ]
        ]


lorem : String
lorem =
    """Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."""


iroha : String
iroha =
    "いろはにほへと散りぬるをわかよ誰そ常ならむ有為の奥山今日越えてあさきゆめみしゑひもせすん"


border : Html Msg
border =
    section []
        [ h1 [ sizeSection ] [ t "Border" ]
        , withSource "div [ padding5, Border.solid, Border.w1 ] [ t \"I'm surrounded by solid border.\" ]" <|
            div [ padding5, Border.solid, Border.w1 ] [ t "I'm surrounded by solid border." ]
        , withSource "div [ padding5, Border.dotted, Border.w1 ] [ t \"I'm surrounded by dotted border.\" ]" <|
            div [ padding5, Border.dotted, Border.w1 ] [ t "I'm surrounded by dotted border." ]
        , withSource "div [ padding5, Border.dashed, Border.w1 ] [ t \"I'm surrounded by dashed border.\" ]" <|
            div [ padding5, Border.dashed, Border.w1 ] [ t "I'm surrounded by dashed border." ]
        , withSource "div [ padding5, Border.solid, Border.w1, Border.round2 ] [ t \"I'm surrounded by rounded solid border.\" ]" <|
            div [ padding5, Border.solid, Border.w1, Border.round2 ] [ t "I'm surrounded by rounded solid border." ]
        , withSource "div [ padding5, Border.solid, Border.w1, Border.round5 ] [ t \"I'm surrounded by more rounded solid border.\" ]" <|
            div [ padding5, Border.solid, Border.w1, Border.round5 ] [ t "I'm surrounded by more rounded solid border." ]
        , withSource """div [ growRow ]
    [ div [ Border.leftRound5, padding10, Background.colorSucc ] [ t "I'm left, and border-rounded." ]
    , div [ padding10, Background.colorWarn ] [ t "I'm center, and not border-rounded." ]
    , div [ Border.rightRound5, padding10, Background.colorErr ] [ t "I'm right, and border-rounded." ]
    ]""" <|
            div [ growRow ]
                [ div [ Border.leftRound5, padding10, Background.colorSucc ] [ t "I'm left, and border-rounded." ]
                , div [ padding10, Background.colorWarn ] [ t "I'm center, and not border-rounded." ]
                , div [ Border.rightRound5, padding10, Background.colorErr ] [ t "I'm right, and border-rounded." ]
                ]
        , withSource """div [ Border.gutter, padding10, Background.colorBg, style "height" "100px" ] [ t "I'm a guttered block." ]""" <|
            div [ Border.gutter, padding10, Background.colorBg ] [ t "I'm a guttered block." ]
        , withSource """div
    [ Border.gutter
    , Border.color oneDarkTheme.prim
    , padding10
    , Background.colorBg
    ]
    [ t "Gutter border colors can be specified by "
    , code [] [ t "Border.color" ]
    , t " helper."
    ]""" <|
            div
                [ Border.gutter
                , Border.color oneDarkTheme.prim
                , padding10
                , Background.colorBg
                ]
                [ t "Gutter border colors can be specified by "
                , code [] [ t "Border.color" ]
                , t " helper."
                ]
        ]


background : Html Msg
background =
    let
        themedBg theme_ themeText =
            section [ theme_ ]
                [ h2 [ sizeTitle ] [ t themeText ]
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
    in
    section []
        [ h1 [ sizeSection ] [ t "Background" ]
        , themedBg oneDark "oneDark"
        , themedBg aubergine "aubergine"
        ]


layout : Html Msg
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


flexBox : Html Msg
flexBox =
    section []
        [ withSource """div [ widthFill, Border.solid, Border.w1 ]
    [ t "I eat all available width. "
    , t "Though, since I am a child of "
    , code [] [ t "growRow" ]
    , t ", I am automatically stretched horizontally."
    ]""" <|
            div [ widthFill, Border.solid, Border.w1 ]
                [ t "I eat all available width. "
                , t "Though, since I am a child of "
                , code [] [ t "growRow" ]
                , t ", I am automatically stretched horizontally."
                ]
        , withSource """div [ flexRow ]
    [ div [ Border.solid, Border.w1 ] [ t "I shrink as narrow as content length allows." ]
    , div [ Border.solid, Border.w1, style "flex-basis" "200px" ] [ t "I am fixed 200px width." ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex row. I grow." ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are vertically stretched by default." ]
    ]""" <|
            div [ flexRow ]
                [ div [ Border.solid, Border.w1 ] [ t "I shrink as narrow as content length allows." ]
                , div [ Border.solid, Border.w1, style "flex-basis" "200px" ] [ t "I am fixed 200px width." ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex row. I grow." ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are vertically stretched by default." ]
                ]
        , withSource """div [ flexColumn, style "height" "30vh" ]
    [ div [ Border.solid, Border.w1 ] [ t "I shrink as short as content length allows." ]
    , div [ Border.solid, Border.w1, style "flex-basis" "200px" ] [ t "I am fixed 200px height." ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex column. I grow." ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are horizontally stretched by default." ]
    ]""" <|
            div [ flexColumn, style "height" "30vh" ]
                [ div [ Border.solid, Border.w1 ] [ t "I shrink as short as content length allows." ]
                , div [ Border.solid, Border.w1, style "flex-basis" "200px" ] [ t "I am fixed 200px height." ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex column. I grow." ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are horizontally stretched by default." ]
                ]
        , withSource """div [ growRow ]
    [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growRow" ], t " automatically grow." ]
    , div [ Border.solid, Border.w1 ] [ t "Like this." ]
    , div [ Border.solid, Border.w1 ] [ t "Like us." ]
    ]""" <|
            div [ growRow ]
                [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growRow" ], t " automatically grow." ]
                , div [ Border.solid, Border.w1 ] [ t "Like this." ]
                , div [ Border.solid, Border.w1 ] [ t "Like us." ]
                ]
        , withSource """div [ growColumn, style "height" "30vh" ]
    [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growColumn" ], t " automatically grow." ]
    , div [ Border.solid, Border.w1 ] [ t "Like this." ]
    , div [ Border.solid, Border.w1 ] [ t "Like us." ]
    ]""" <|
            div [ growColumn, style "height" "30vh" ]
                [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growColumn" ], t " automatically grow." ]
                , div [ Border.solid, Border.w1 ] [ t "Like this." ]
                , div [ Border.solid, Border.w1 ] [ t "Like us." ]
                ]
        , withSource """div [ growRow, flexCenter ]
    [ div [ Border.solid, Border.w1 ] [ t "We are vertically centered and not stretched." ]
    , div [ Border.solid, Border.w1 ] [ t (String.repeat 3 lorem) ]
    , div [ Border.solid, Border.w1 ] [ t (String.repeat 3 iroha) ]
    ]""" <|
            div [ growRow, flexCenter ]
                [ div [ Border.solid, Border.w1 ] [ t "We are vertically centered and not stretched." ]
                , div [ Border.solid, Border.w1 ] [ t (String.repeat 3 lorem) ]
                , div [ Border.solid, Border.w1 ] [ t (String.repeat 3 iroha) ]
                ]
        , withSource """div [ growColumn, flexCenter ]
    [ div [ Border.solid, Border.w1 ] [ t "We are horizontally centered and not stretched." ]
    , div [ Border.solid, Border.w1 ] [ t lorem ]
    , div [ Border.solid, Border.w1 ] [ t iroha ]
    ]""" <|
            div [ growColumn, flexCenter ]
                [ div [ Border.solid, Border.w1 ] [ t "We are horizontally centered and not stretched." ]
                , div [ Border.solid, Border.w1 ] [ t lorem ]
                , div [ Border.solid, Border.w1 ] [ t iroha ]
                ]
        ]


padding : Html Msg
padding =
    section []
        [ withSource """div [ Border.solid, Border.w1 ] [ t "No padding. ", t lorem ]""" <|
            div [ Border.solid, Border.w1 ] [ t "No padding. ", t lorem ]
        , withSource """div [ padding2, Border.solid, Border.w1 ] [ t "I'm surrounded by 2px padding. ", t lorem ]""" <|
            div [ padding2, Border.solid, Border.w1 ] [ t "I'm surrounded by 2px padding. ", t lorem ]
        , withSource """div [ padding5, Border.solid, Border.w1 ] [ t "I'm surrounded by 5px padding. ", t lorem ]""" <|
            div [ padding5, Border.solid, Border.w1 ] [ t "I'm surrounded by 5px padding. ", t lorem ]
        , withSource """div [ padding10, Border.solid, Border.w1 ] [ t "I'm surrounded by 10px padding. ", t lorem ]""" <|
            div [ padding10, Border.solid, Border.w1 ] [ t "I'm surrounded by 10px padding. ", t lorem ]
        , withSource """div [ padding15, Border.solid, Border.w1 ] [ t "I'm surrounded by 15px padding. ", t lorem ]""" <|
            div [ padding15, Border.solid, Border.w1 ] [ t "I'm surrounded by 15px padding. ", t lorem ]
        ]


spacing : Html Msg
spacing =
    section []
        [ withSource """div [ growRow, Border.solid, Border.w1 ]
    [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex row." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the second one. No spacing." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
    ]""" <|
            div [ growRow, Border.solid, Border.w1 ]
                [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex row." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the second one. No spacing." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
                ]
        , withSource """div [ growRow, spacingRow5, Border.solid, Border.w1 ]
    [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex row." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have left margins." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
    ]""" <|
            div [ growRow, spacingRow5, Border.solid, Border.w1 ]
                [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex row." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have left margins." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
                ]
        , withSource """div [ growRow, spacingRow15, Border.solid, Border.w1 ]
    [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex row." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have left margins." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
    ]""" <|
            div [ growRow, spacingRow15, Border.solid, Border.w1 ]
                [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex row." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have left margins." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
                ]
        , withSource """div [ growColumn, Border.solid, Border.w1 ]
    [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex column." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the second one. No spacing." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
    ]""" <|
            div [ growColumn, Border.solid, Border.w1 ]
                [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex column." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the second one. No spacing." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
                ]
        , withSource """div [ growColumn, spacingColumn5, Border.solid, Border.w1 ]
    [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex column." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have top margins." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
    ]""" <|
            div [ growColumn, spacingColumn5, Border.solid, Border.w1 ]
                [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex column." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have top margins." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
                ]
        , withSource """div [ growColumn, spacingColumn15, Border.solid, Border.w1 ]
    [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex column." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have top margins." ]
    , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
    ]""" <|
            div [ growColumn, spacingColumn15, Border.solid, Border.w1 ]
                [ div [ Border.solid, Border.w1 ] [ t "I'm the first child of flex column." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the second one. Second and thereafter have top margins." ]
                , div [ Border.solid, Border.w1 ] [ t "I'm the third one." ]
                ]
        ]


button_ : Html Msg
button_ =
    let
        themedButtons theme_ themeText =
            section [ theme_ ]
                [ h2 [ sizeTitle ] [ t themeText ]
                , withSource """button [ Button.prim, widthFill ] [ t "prim" ]""" <| button [ Button.prim, widthFill ] [ t "prim" ]
                , withSource """button [ Button.succ, widthFill ] [ t "succ" ]""" <| button [ Button.succ, widthFill ] [ t "succ" ]
                , withSource """button [ Button.warn, widthFill ] [ t "warn" ]""" <| button [ Button.warn, widthFill ] [ t "warn" ]
                , withSource """button [ Button.err, widthFill ] [ t "err" ]""" <| button [ Button.err, widthFill ] [ t "err" ]
                ]
    in
    section []
        [ h1 [ sizeSection ] [ t "Button" ]
        , themedButtons oneDark "oneDark"
        , themedButtons aubergine "aubergine"
        ]
