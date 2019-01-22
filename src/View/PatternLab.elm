module View.PatternLab exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Octicons
import StringExtra
import Url exposing (Url)
import Url.Builder
import Url.Parser as U
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Button as Button
import View.Atom.Image as Image
import View.Atom.Input as Input
import View.Atom.Input.Select as Select
import View.Atom.Layout exposing (..)
import View.Atom.TextBlock exposing (forceBreak)
import View.Atom.Theme exposing (aubergine, oneDark, oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Organism.Config.Pref as Pref
import View.Organism.Sidebar as Sidebar
import View.Style exposing (none, px)
import View.Stylesheet
import View.Template.Main exposing (DragStatus(..))


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Select.sub SelectCtrl m.select
        , onUrlRequest = GoTo
        , onUrlChange = Arrived
        }


type alias Model =
    { key : Key
    , route : Route
    , textInput : String
    , toggle : Bool
    , select : Select.State
    , selected : Maybe String
    , numColumns : Int
    }


type Route
    = Top
    | Typography
    | TextBlock
    | Border
    | Background
    | Layout
    | Image
    | Button
    | Input
    | Icon
    | Sidebar
    | ConfigPref
    | MainTemplate


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , route = urlToRoute url
      , textInput = ""
      , toggle = False
      , select = Select.AllClosed
      , selected = Nothing
      , numColumns = 10
      }
    , Cmd.none
    )


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
                , U.map Image (U.s "image")
                , U.map Button (U.s "button")
                , U.map Input (U.s "input")
                , U.map Icon (U.s "icon")
                , U.map Sidebar (U.s "sidebar")
                , U.map ConfigPref (U.s "config_pref")
                , U.map MainTemplate (U.s "main_template")
                ]
    in
    Maybe.withDefault Top (U.parse urlParser url)


type Msg
    = NoOp
    | GoTo Browser.UrlRequest
    | Arrived Url
    | TextInput String
    | Toggle Bool
    | SelectCtrl (Select.Msg Msg)
    | Selected String
    | AddColumn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        GoTo (Browser.Internal url) ->
            ( m, Browser.Navigation.pushUrl m.key (Url.toString url) )

        GoTo (Browser.External urlStr) ->
            ( m, Browser.Navigation.load urlStr )

        TextInput str ->
            ( { m | textInput = str }, Cmd.none )

        Arrived url ->
            ( { m | route = urlToRoute url }, Cmd.none )

        Toggle bool ->
            ( { m | toggle = bool }, Cmd.none )

        SelectCtrl sMsg ->
            let
                ( ss, cmd ) =
                    Select.update SelectCtrl sMsg m.select
            in
            ( { m | select = ss }, cmd )

        Selected opt ->
            ( { m | selected = Just opt }, Cmd.none )

        AddColumn ->
            ( { m | numColumns = m.numColumns + 1 }, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view m =
    let
        pLab contents =
            [ div [ flexColumn, widthFill, spacingColumn15, oneDark ] (navi m.route :: contents) ]
    in
    { title = "Zephyr: Pattern Lab"
    , body =
        (::) View.Stylesheet.render <|
            case m.route of
                Top ->
                    pLab [ introduction, theme ]

                Typography ->
                    pLab [ typography ]

                TextBlock ->
                    pLab [ textBlock ]

                Border ->
                    pLab [ border ]

                Background ->
                    pLab [ background ]

                Layout ->
                    pLab [ layout ]

                Image ->
                    pLab [ image ]

                Button ->
                    pLab [ button_ ]

                Input ->
                    pLab [ input_ m ]

                Icon ->
                    pLab [ icon ]

                Sidebar ->
                    pLab [ sidebar m ]

                ConfigPref ->
                    pLab [ configPref m ]

                MainTemplate ->
                    mainTemplate m
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
            , naviButton r Image "Image"
            , naviButton r Button "Button"
            , naviButton r Input "Input"
            ]
        , div [ flexRow, flexCenter, spacingRow15 ]
            [ h2 [ sizeHeadline, bold ] [ t "Molecules" ]
            , naviButton r Icon "Icon"
            ]
        , div [ flexRow, flexCenter, spacingRow15 ]
            [ h2 [ sizeHeadline, bold ] [ t "Organisms" ]
            , naviButton r Sidebar "Sidebar"
            , naviButton r ConfigPref "Config.Pref"
            ]
        , div [ flexRow, flexCenter, spacingRow15 ]
            [ h2 [ sizeHeadline, bold ] [ t "Templates" ]
            , naviButton r MainTemplate "Main"
            ]
        ]


naviButton : Route -> Route -> String -> Html Msg
naviButton current hit btnLabel =
    let
        c =
            if current == hit then
                Background.colorSucc

            else
                Background.colorPrim
    in
    Button.link [ sizeHeadline, padding10, flexItem, c ]
        { url = routeToString hit, children = [ t btnLabel ] }


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

        Image ->
            abs_ [ "image" ]

        Button ->
            abs_ [ "button" ]

        Input ->
            abs_ [ "input" ]

        Icon ->
            abs_ [ "icon" ]

        Sidebar ->
            abs_ [ "sidebar" ]

        ConfigPref ->
            abs_ [ "config_pref" ]

        MainTemplate ->
            abs_ [ "main_template" ]


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
import Html.Attributes exposing (..)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Button as Button
import View.Atom.Image as Image
import View.Atom.Input as Input
import View.Atom.Input.Select as Select
import View.Atom.Layout exposing (..)
import View.Atom.TextBlock exposing (forceBreak)
import View.Atom.Theme exposing (aubergine, oneDark, oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Style exposing (px)"""
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
                , withSource """p []
    [ t "Links pop on the current tab unless specified as "
    , code [] [ t "newTab" ]
    , t " "
    , link [ newTab ] { url = "https://example.com", children = [ t "example.com" ] }
    ]""" <|
                    p []
                        [ t "Links pop on the current tab unless specified as "
                        , code [] [ t "newTab" ]
                        , t " "
                        , link [ newTab ] { url = "https://example.com", children = [ t "example.com" ] }
                        ]
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
        , withSource """div [ padding5, Border.solid, Border.w1, Border.elliptic ] [ t "I'm elliptic!" ]""" <|
            div [ padding5, Border.solid, Border.w1, Border.elliptic ] [ t "I'm elliptic!" ]
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
    let
        basics =
            section []
                [ h2 [ sizeTitle ] [ t "Basics" ]
                , withSource """div [ Border.solid, Border.w1 ] [ t "I am a bare ", code [] [ t "div" ] ]""" <|
                    div [ Border.solid, Border.w1, flexShrink ] [ t "I am a bare ", code [] [ t "div" ] ]
                , withSource """div [ widthFill, Border.solid, Border.w1 ]
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
                , withSource """div [ Border.solid, Border.w1 ]
    [ code [] [ t "block" ]
    , t " enforces any elements to become a block element, like this:"
    , code [ block ] [ t "I'm blockified." ]
    ]""" <|
                    div [ Border.solid, Border.w1 ]
                        [ code [] [ t "block" ]
                        , t " enforces any elements to become a block element, like this:"
                        , code [ block ] [ t "I'm blockified." ]
                        ]
                ]
    in
    section []
        [ h1 [ sizeSection ] [ t "Layout" ]
        , basics
        , flexBox
        , padding
        , spacing
        , badge
        ]


flexBox : Html Msg
flexBox =
    section []
        [ h2 [ sizeTitle ] [ t "FlexBox" ]
        , withSource """div [ flexRow ]
    [ div [ Border.solid, Border.w1 ] [ t "I shrink as narrow as content length allows." ]
    , div [ Border.solid, Border.w1, flexBasis (px 200) ] [ t "I am fixed 200px width. ", t lorem ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex row. I grow. ", t lorem ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are vertically stretched by default. ", t lorem ]
    ]""" <|
            div [ flexRow ]
                [ div [ Border.solid, Border.w1 ] [ t "I shrink as narrow as content length allows." ]
                , div [ Border.solid, Border.w1, flexBasis (px 200) ] [ t "I am fixed 200px width. ", t lorem ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex row. I grow. ", t lorem ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are vertically stretched by default. ", t lorem ]
                ]
        , withSource """div [ flexColumn ]
    [ div [ Border.solid, Border.w1 ] [ t "I shrink as short as content length allows. ", t lorem ]
    , div [ Border.solid, Border.w1, flexShrink, flexBasis (px 200) ] [ t "I start from 200px height, but may shrink. ", t lorem ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex column. I grow. ", t lorem ]
    , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are horizontally stretched by default. ", t lorem ]
    ]""" <|
            div [ flexColumn ]
                [ div [ Border.solid, Border.w1 ] [ t "I shrink as short as content length allows. ", t lorem ]
                , div [ Border.solid, Border.w1, flexShrink, flexBasis (px 200) ] [ t "I start from 200px height, but may shrink. ", t lorem ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "We are children of flex column. I grow. ", t lorem ]
                , div [ flexGrow, Border.solid, Border.w1 ] [ t "I grow too. Also, we are horizontally stretched by default. ", t lorem ]
                ]
        , withSource """div [ growRow ]
    [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growRow" ], t " automatically grow. ", t lorem ]
    , div [ Border.solid, Border.w1 ] [ t "Like this. ", t lorem ]
    , div [ Border.solid, Border.w1 ] [ t "Like us. ", t lorem ]
    ]""" <|
            div [ growRow ]
                [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growRow" ], t " automatically grow. ", t lorem ]
                , div [ Border.solid, Border.w1 ] [ t "Like this. ", t lorem ]
                , div [ Border.solid, Border.w1 ] [ t "Like us. ", t lorem ]
                ]
        , withSource """div [ growColumn, style "height" (px 300) ]
    [ div [ Border.solid, Border.w1 ] [ t "Children of ", code [] [ t "growColumn" ], t " automatically grow." ]
    , div [ Border.solid, Border.w1 ] [ t "Like this." ]
    , div [ Border.solid, Border.w1 ] [ t "Like us." ]
    ]""" <|
            div [ growColumn, style "height" (px 300) ]
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
        [ h2 [ sizeTitle ] [ t "Padding" ]
        , withSource """div [ Border.solid, Border.w1 ] [ t "No padding. ", t lorem ]""" <|
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
        [ h2 [ sizeTitle ] [ t "Spacing" ]
        , withSource """div [ growRow, Border.solid, Border.w1 ]
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


badge : Html Msg
badge =
    section []
        [ h2 [ sizeTitle ] [ t "Badge" ]
        , withSource """withBadge []
    { content =
        div [ padding2 ]
            [ div [ Background.colorNote, padding10 ] [ t "I'm main content. This is a special layouting helper Atom for badges. ", t lorem ]
            ]
    , topRight =
        Just (div [ Background.colorSucc ] [ t "I'm top-right badge!" ])
    , bottomRight =
        Just (div [ Background.colorErr ] [ t "I'm bottom-right badge!" ])
    }""" <|
            withBadge []
                { content =
                    div [ padding2 ]
                        [ div [ Background.colorNote, padding10 ] [ t "I'm main content. This is a special layouting helper Atom for badges. ", t lorem ]
                        ]
                , topRight =
                    Just (div [ Background.colorSucc ] [ t "I'm top-right badge!" ])
                , bottomRight =
                    Just (div [ Background.colorErr ] [ t "I'm bottom-right badge!" ])
                }
        , withSource """withBadge []
    { content = div [ padding2 ] [ div [ Background.colorNote, padding10 ] [ t lorem ] ]
    , topRight = Just (div [ Background.colorSucc ] [ t "Top-right only" ])
    , bottomRight = Nothing
    }""" <|
            withBadge []
                { content = div [ padding2 ] [ div [ Background.colorNote, padding10 ] [ t lorem ] ]
                , topRight = Just (div [ Background.colorSucc ] [ t "Top-right only" ])
                , bottomRight = Nothing
                }
        , withSource """withBadge []
    { content = div [ padding2 ] [ div [ Background.colorNote, padding10 ] [ t lorem ] ]
    , topRight = Nothing
    , bottomRight = Just (div [ Background.colorErr ] [ t "Bottom-right only" ])
    }""" <|
            withBadge []
                { content = div [ padding2 ] [ div [ Background.colorNote, padding10 ] [ t lorem ] ]
                , topRight = Nothing
                , bottomRight = Just (div [ Background.colorErr ] [ t "Bottom-right only" ])
                }
        ]


image : Html Msg
image =
    section []
        [ h1 [ sizeSection ] [ t "Image" ]
        , withSource """img [ src (Image.ph 200 100), alt "200x100", width 200, height 100 ] []""" <|
            img [ src (Image.ph 200 100), alt "200x100", width 200, height 100 ] []
        , withSource """div []
    [ t "By default, "
    , code [] [ t "<img>" ]
    , t "s are inline elements. "
    , img [ src (Image.ph 20 20), alt "20x20", width 20, height 20 ] []
    , t " "
    , img [ src (Image.ph 20 20), alt "20x20", width 20, height 20 ] []
    , t "You see that they are aligned by "
    , code [] [ t "vertical-align: middle;" ]
    , t " which is the global default. "
    , img [ src (Image.ph 30 15), alt "30x15", width 30, height 15 ] []
    , t "水兵リーベ。"
    ]""" <|
            div []
                [ t "By default, "
                , code [] [ t "<img>" ]
                , t "s are inline elements. "
                , img [ src (Image.ph 20 20), alt "20x20", width 20, height 20 ] []
                , t " "
                , img [ src (Image.ph 20 20), alt "20x20", width 20, height 20 ] []
                , t "You see that they are aligned by "
                , code [] [ t "vertical-align: middle;" ]
                , t " which is the global default. "
                , img [ src (Image.ph 30 15), alt "30x15", width 30, height 15 ] []
                , t "水兵リーベ。"
                ]
        , withSource """div []
    [ t "Can become a block element with "
    , code [] [ t "block" ]
    , img [ src (Image.ph 400 300), alt "400x300", width 400, height 300, block ] []
    ]""" <|
            div []
                [ t "Can become a block element with "
                , code [] [ t "block" ]
                , img [ src (Image.ph 400 300), alt "400x300", width 400, height 300, block ] []
                ]
        , withSource """div [ growRow, spacingRow10 ]
    [ p [] [ t "Or, for finer control, just become a flex item! ", t lorem ]
    , img [ src (Image.ph 200 200), alt "200x200", width 200, height 200, flexItem, flexBasis (px 200) ] []
    , p [] [ t "Use ", code [] [ t "flexBasis" ], t " if needs be. ", t (String.repeat 5 iroha) ]
    ]""" <|
            div [ growRow, spacingRow10 ]
                [ p [] [ t "Or, for finer control, just become a flex item! ", t lorem ]
                , img [ src (Image.ph 200 200), alt "200x200", width 200, height 200, flexItem, flexBasis (px 200) ] []
                , p [] [ t "Use ", code [] [ t "flexBasis" ], t " if needs be. ", t (String.repeat 5 iroha) ]
                ]
        , withSource """div []
    [ t "Can be rounded"
    , img [ src (Image.ph 300 300), alt "300x300", width 300, height 300, block, Border.round5 ] []
    ]""" <|
            div []
                [ t "Can be rounded"
                , img [ src (Image.ph 300 300), alt "300x300", width 300, height 300, block, Border.round5 ] []
                ]
        , withSource """div []
    [ t "Can be circled even!"
    , img [ src (Image.ph 300 300), alt "300x300", width 300, height 300, block, Border.elliptic ] []
    ]""" <|
            div []
                [ t "Can be circled even!"
                , img [ src (Image.ph 300 300), alt "300x300", width 300, height 300, block, Border.elliptic ] []
                ]
        , withSource """div []
    [ t "Octicon API is also provided. "
    , Image.octicon { size = 20, shape = Octicons.alert }
    , t " They are also inline elements. "
    , Image.octicon { size = 20, shape = Octicons.info }
    ]""" <|
            div []
                [ t "Octicon API is also provided. "
                , Image.octicon { size = 20, shape = Octicons.alert }
                , t " They are also inline elements. "
                , Image.octicon { size = 20, shape = Octicons.info }
                ]
        , withSource """div []
    [ t "Octicon fill colors depend on upstream theme and custom styles. "
    , div [ oneDark ] [ Image.octicon { size = 30, shape = Octicons.star } ]
    , div [ aubergine ] [ Image.octicon { size = 30, shape = Octicons.star } ]
    ]""" <|
            div []
                [ t "Octicon fill colors depend on upstream theme and custom styles. "
                , div [ oneDark ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                , div [ aubergine ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                ]
        ]


button_ : Html Msg
button_ =
    let
        themedStdButtons theme_ themeText =
            section [ theme_ ]
                [ h2 [ sizeTitle ] [ t themeText ]
                , withSource """button [] [ t "default" ]""" <| button [] [ t "default" ]
                , withSource """button [ Background.colorPrim ] [ t "prim" ]""" <| button [ Background.colorPrim ] [ t "prim" ]
                , withSource """button [ Background.colorSucc ] [ t "succ" ]""" <| button [ Background.colorSucc ] [ t "succ" ]
                , withSource """button [ Background.colorWarn ] [ t "warn" ]""" <| button [ Background.colorWarn ] [ t "warn" ]
                , withSource """button [ Background.colorErr ] [ t "err" ]""" <| button [ Background.colorErr ] [ t "err" ]
                , withSource """button [ Background.colorPrim, sizeHeadline ] [ t "prim" ]""" <| button [ Background.colorPrim, sizeHeadline ] [ t "prim HEADLINE" ]
                , withSource """button [ Background.colorPrim, sizeSection ] [ t "prim" ]""" <| button [ Background.colorPrim, sizeSection ] [ t "prim SECTION" ]
                , withSource """button [ Background.colorPrim, widthFill ] [ t "Can fill with ", code [] [ t "widthFill" ] ]""" <|
                    button [ Background.colorPrim, widthFill ] [ t "Can fill with ", code [] [ t "widthFill" ] ]
                , withSource """button [ Background.colorPrim, padding10 ]
    [ t "Can be padded with "
    , code [] [ t "padding10" ]
    , t " and its family"
    ]""" <|
                    button [ Background.colorPrim, padding10 ]
                        [ t "Can be padded with "
                        , code [] [ t "padding10" ]
                        , t " and its family"
                        ]
                , withSource """button [ Background.colorPrim, disabled True ] [ t "I'm disabled" ]""" <|
                    button [ Background.colorPrim, disabled True ] [ t "I'm disabled" ]
                , withSource """button [ Background.colorPrim, Border.noRound ] [ t "Rounding can be canceled" ]""" <|
                    button [ Background.colorPrim, Border.noRound ] [ t "Rounding can be canceled" ]
                , withSource """button [ Background.colorPrim, Border.round5 ] [ t "Explicit rounding can be set" ]""" <|
                    button [ Background.colorPrim, Border.round5 ] [ t "Explicit rounding can be set" ]
                , withSource """button [ Background.colorPrim, Border.w1, Border.solid ] [ t "Can have borders" ]""" <|
                    button [ Background.colorPrim, Border.w1, Border.solid ] [ t "Can have borders" ]
                , withSource """div []
    [ code [] [ t "<button>" ]
    , t " is an inline element. "
    , button [ Background.colorPrim ] [ t "Like This" ]
    ]""" <|
                    div []
                        [ code [] [ t "<button>" ]
                        , t " is an inline element. "
                        , button [ Background.colorPrim ] [ t "Like This" ]
                        ]
                , withSource """div [ flexRow ] [ button [ Background.colorPrim, flexGrow, flexItem ] [ t "Can become a flex item" ] ]""" <|
                    div [ flexRow ] [ button [ Background.colorPrim, flexGrow, flexItem ] [ t "Can become a flex item" ] ]
                , withSource """div [ growRow, sizeHeadline ]
    [ button [ Background.colorSucc, Border.leftRound5 ] [ t "We can achieve coupled button row" ]
    , button [ Background.colorWarn, Border.noRound ] [ t "We can achieve coupled button row" ]
    , button [ Background.colorErr, Border.rightRound5 ] [ t "We can achieve coupled button row" ]
    ]""" <|
                    div [ growRow, sizeHeadline ]
                        [ button [ Background.colorSucc, Border.leftRound5 ] [ t "We can achieve coupled button row" ]
                        , button [ Background.colorWarn, Border.noRound ] [ t "We can achieve coupled button row" ]
                        , button [ Background.colorErr, Border.rightRound5 ] [ t "We can achieve coupled button row" ]
                        ]
                ]

        themedLinkButtons theme_ themeText =
            section [ theme_ ]
                [ h2 [ sizeTitle ] [ t themeText ]
                , withSource """Button.link [] { url = "https://example.com", children = [ t "A Link button looks like a button but is a link" ] }""" <|
                    Button.link [] { url = "https://example.com", children = [ t "A Link button looks like a button but is a link" ] }
                , withSource """Button.link [ Background.colorPrim ] { url = "https://example.com", children = [ t "colorPrim" ] }""" <|
                    Button.link [ Background.colorPrim ] { url = "https://example.com", children = [ t "colorPrim" ] }
                , withSource """Button.link [ Background.colorSucc ] { url = "https://example.com", children = [ t "colorSucc" ] }""" <|
                    Button.link [ Background.colorSucc ] { url = "https://example.com", children = [ t "colorSucc" ] }
                , withSource """Button.link [ Background.colorWarn ] { url = "https://example.com", children = [ t "colorWarn" ] }""" <|
                    Button.link [ Background.colorWarn ] { url = "https://example.com", children = [ t "colorWarn" ] }
                , withSource """Button.link [ Background.colorErr ] { url = "https://example.com", children = [ t "colorErr" ] }""" <|
                    Button.link [ Background.colorErr ] { url = "https://example.com", children = [ t "colorErr" ] }
                , withSource """Button.link [ Background.colorPrim, padding10, sizeSection, Border.w1, Border.solid ]
    { url = "https://example.com"
    , children = [ t "Can be padded/sized/bordered" ]
    }""" <|
                    Button.link [ Background.colorPrim, padding10, sizeSection, Border.w1, Border.solid ]
                        { url = "https://example.com"
                        , children = [ t "Can be padded/sized/bordered" ]
                        }
                , withSource """Button.link
    [ Background.colorPrim, padding5, newTab ]
    { url = "https://example.com"
    , children = [ code [] [ t "newTab" ], t " also works" ]
    }""" <|
                    Button.link
                        [ Background.colorPrim, padding5, newTab ]
                        { url = "https://example.com"
                        , children = [ code [] [ t "newTab" ], t " also works" ]
                        }
                ]
    in
    section []
        [ h1 [ sizeSection ] [ t "Button" ]
        , themedStdButtons oneDark "oneDark"
        , themedStdButtons aubergine "aubergine"
        , h1 [ sizeSection ] [ t "Link Button" ]
        , themedLinkButtons oneDark "oneDark"
        , themedLinkButtons aubergine "aubergine"
        ]


input_ : Model -> Html Msg
input_ m =
    section []
        [ h1 [ sizeSection ] [ t "Input" ]
        , textInput m.textInput
        , toggle m.toggle
        , select_ m.select m.selected
        ]


textInput : String -> Html Msg
textInput currentInput =
    section []
        [ h2 [ sizeTitle ] [ t "Text" ]
        , withSource """div [] [ t "Inline text input. ", input [ type_ "text", value currentInput, onInput TextInput ] [] ]""" <|
            div [] [ t "Inline text input. ", input [ type_ "text", value currentInput, onInput TextInput ] [] ]
        , withSource """div [] [ t "With placeholder. ", input [ type_ "text", value currentInput, onInput TextInput, placeholder "Write something!" ] [] ]""" <|
            div [] [ t "With placeholder. ", input [ type_ "text", value currentInput, onInput TextInput, placeholder "Write something!" ] [] ]
        , withSource """div []
    [ t "Styles/layouts attached. "
    , input
        [ type_ "text"
        , value currentInput
        , onInput TextInput
        , placeholder "Big input"
        , block
        , sizeHeadline
        , padding5
        , Border.round5
        , Border.w1
        , Border.solid
        , style "width" (px 300)
        ]
        []
    ]""" <|
            div []
                [ t "Styles/layouts attached. "
                , input
                    [ type_ "text"
                    , value currentInput
                    , onInput TextInput
                    , placeholder "Big input"
                    , block
                    , sizeHeadline
                    , padding5
                    , Border.round5
                    , Border.w1
                    , Border.solid
                    , style "width" (px 300)
                    ]
                    []
                ]
        , withSource """div [ aubergine ]
    [ t "Can be themed."
    , input [ type_ "text", value currentInput, onInput TextInput, placeholder "Write something!", block ] []
    ]""" <|
            div [ aubergine ]
                [ t "Can be themed."
                , input [ type_ "text", value currentInput, onInput TextInput, placeholder "Write something!", block ] []
                ]
        ]


toggle : Bool -> Html Msg
toggle checked =
    section []
        [ h2 [ sizeTitle ] [ t "Toggle" ]
        , withSource """div []
    [ t "Inline toggle input. Width and height are fixed 36x18px. "
    , Input.toggle [] { onChange = Toggle, checked = checked }
    ]""" <|
            div []
                [ t "Inline toggle input. Width and height are fixed 36x18px. "
                , Input.toggle [] { onChange = Toggle, checked = checked }
                ]
        , withSource """div []
    [ t "As a block element. "
    , Input.toggle [ block ] { onChange = Toggle, checked = checked }
    ]""" <|
            div []
                [ t "As a block element. "
                , Input.toggle [ block ] { onChange = Toggle, checked = checked }
                ]
        , withSource """label []
    [ t "With Label!"
    , Input.toggle [ block ] { onChange = Toggle, checked = checked }
    ]""" <|
            label []
                [ t "With Label!"
                , Input.toggle [ block ] { onChange = Toggle, checked = checked }
                ]
        , withSource """div [ aubergine ]
    [ t "Can be themed!"
    , Input.toggle [ block ] { onChange = Toggle, checked = checked }
    ]""" <|
            div [ aubergine ]
                [ t "Can be themed!"
                , Input.toggle [ block ] { onChange = Toggle, checked = checked }
                ]
        ]


select_ : Select.State -> Maybe String -> Html Msg
select_ ss selected =
    let
        options =
            List.range 0 20
                |> List.map
                    (\int ->
                        Tuple.pair ("option" ++ String.fromInt int) <|
                            case modBy 4 int of
                                0 ->
                                    "abcd0123"

                                1 ->
                                    "水兵リーベ。"

                                2 ->
                                    lorem

                                _ ->
                                    iroha
                    )
    in
    section []
        [ h2 [ sizeTitle ] [ t "Select" ]
        , withSource """div []
    [ t "By default these are block elements."
    , Select.select []
        { state = ss
        , msgTagger = SelectCtrl
        , id = "s1"
        , thin = False
        , onSelect = Selected
        , selectedOption = selected
        , filterMatch = Nothing
        , options = options
        , optionHtml = text
        }
    ]""" <|
            div []
                [ t "By default these are block elements."
                , Select.select []
                    { state = ss
                    , msgTagger = SelectCtrl
                    , id = "s1"
                    , thin = False
                    , onSelect = Selected
                    , selectedOption = selected
                    , filterMatch = Nothing
                    , options = options
                    , optionHtml = text
                    }
                ]
        , withSource """div [ growRow, spacingRow10 ]
    [ p [] [ t "Width can be contained externally. Be warned though, flex calculation with padding is complicated, and may not work as you intended!" ]
    , Select.select [ style "max-width" "300px" ]
        { state = ss
        , msgTagger = SelectCtrl
        , id = "s2"
        , thin = False
        , onSelect = Selected
        , selectedOption = selected
        , filterMatch = Nothing
        , options = options
        , optionHtml = text
        }
    ]""" <|
            div [ growRow, spacingRow10 ]
                [ p [] [ t "Width can be contained externally. Though, flex calculation with padding may not work as you intended!" ]
                , Select.select [ style "max-width" "300px" ]
                    { state = ss
                    , msgTagger = SelectCtrl
                    , id = "s2"
                    , thin = False
                    , onSelect = Selected
                    , selectedOption = selected
                    , filterMatch = Nothing
                    , options = options
                    , optionHtml = text
                    }
                ]
        , withSource """div []
    [ t "Can be thinned."
    , Select.select []
        { state = ss
        , msgTagger = SelectCtrl
        , id = "s3"
        , thin = True
        , onSelect = Selected
        , selectedOption = selected
        , filterMatch = Nothing
        , options = options
        , optionHtml = text
        }
    ]""" <|
            div []
                [ t "Can be thinned."
                , Select.select []
                    { state = ss
                    , msgTagger = SelectCtrl
                    , id = "s3"
                    , thin = True
                    , onSelect = Selected
                    , selectedOption = selected
                    , filterMatch = Nothing
                    , options = options
                    , optionHtml = text
                    }
                ]
        , withSource """div []
    [ t "Can have filter box."
    , Select.select []
        { state = ss
        , msgTagger = SelectCtrl
        , id = "s4"
        , thin = False
        , onSelect = Selected
        , selectedOption = selected
        , filterMatch = Just String.contains
        , options = options
        , optionHtml = text
        }
    ]""" <|
            div []
                [ t "Can have filter box."
                , Select.select []
                    { state = ss
                    , msgTagger = SelectCtrl
                    , id = "s4"
                    , thin = False
                    , onSelect = Selected
                    , selectedOption = selected
                    , filterMatch = Just String.contains
                    , options = options
                    , optionHtml = text
                    }
                ]
        , withSource """div []
    [ t "Can be themed."
    , Select.select [ aubergine ]
        { state = ss
        , msgTagger = SelectCtrl
        , id = "s5"
        , thin = False
        , onSelect = Selected
        , selectedOption = selected
        , filterMatch = Just String.contains
        , options = options
        , optionHtml = text
        }
    ]""" <|
            div []
                [ t "Can be themed."
                , Select.select [ aubergine ]
                    { state = ss
                    , msgTagger = SelectCtrl
                    , id = "s5"
                    , thin = False
                    , onSelect = Selected
                    , selectedOption = selected
                    , filterMatch = Just String.contains
                    , options = options
                    , optionHtml = text
                    }
                ]
        ]


icon : Html Msg
icon =
    section []
        [ h1 [ sizeSection ] [ t "Icon" ]
        , withSource """Icon.button [] { onPress = NoOp, src = Image.ph 50 50, alt = "50x50 icon" }""" <|
            Icon.button [] { onPress = NoOp, src = Image.ph 50 50, alt = "50x50 icon" }
        , withSource """Icon.button [ Border.round5 ] { onPress = NoOp, src = Image.ph 75 75, alt = "75x75 icon" }""" <|
            Icon.button [ Border.round5 ] { onPress = NoOp, src = Image.ph 75 75, alt = "75x75 icon" }
        , withSource """Icon.link [] { url = "https://example.com", src = Image.ph 50 50, alt = "50x50 icon" }""" <|
            Icon.link [] { url = "https://example.com", src = Image.ph 50 50, alt = "50x50 icon" }
        , withSource """Icon.link [ Border.round5, newTab ] { url = "https://example.com", src = Image.ph 75 75, alt = "75x75 icon" }""" <|
            Icon.link [ Border.round5, newTab ] { url = "https://example.com", src = Image.ph 75 75, alt = "75x75 icon" }
        , withSource """Icon.octiconButton [] { size = 50, onPress = NoOp, shape = Octicons.search }""" <|
            Icon.octiconButton [] { size = 50, onPress = NoOp, shape = Octicons.search }
        , withSource """Icon.octiconButton [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, onPress = NoOp, shape = Octicons.search }""" <|
            Icon.octiconButton [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, onPress = NoOp, shape = Octicons.search }
        , withSource """Icon.octiconLink [] { size = 50, url = "https://example.com", shape = Octicons.rocket }""" <|
            Icon.octiconLink [] { size = 50, url = "https://example.com", shape = Octicons.rocket }
        , withSource """Icon.octiconLink [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, url = "https://example.com", shape = Octicons.rocket }""" <|
            Icon.octiconLink [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, url = "https://example.com", shape = Octicons.rocket }
        ]


sidebar : Model -> Html Msg
sidebar m =
    section []
        [ h1 [ sizeSection ] [ t "Sidebar" ]
        , Sidebar.render (dummySidebarEffects m.toggle) (dummySidebarProps m.toggle m.numColumns)
        , p []
            [ t "Shown to the left. This is a position-width-fixed organism. "
            , t "Msg is not yet wired!"
            ]
        ]


dummySidebarProps : Bool -> Int -> Sidebar.Props
dummySidebarProps isOpen numColumns =
    let
        dummyColumnButton i =
            ( Sidebar.ColumnProps (String.fromInt i) (modBy 2 i == 0)
            , case modBy 3 i of
                0 ->
                    Sidebar.Fallback "Zehpyr"

                1 ->
                    Sidebar.DiscordButton { channelName = "Discord", guildIcon = Just (Image.ph 48 48) }

                _ ->
                    Sidebar.SlackButton { convName = "Slack", teamIcon = Just (Image.ph 50 50) }
            )
    in
    { configOpen = isOpen
    , columns = List.range 0 (numColumns - 1) |> List.map dummyColumnButton
    }


configPref : Model -> Html Msg
configPref m =
    section []
        [ h1 [ sizeSection ] [ t "Config.Pref" ]
        , withSource """let
    dummyShadowColumn index =
        case modBy 3 index of
            0 ->
                ( { id = String.fromInt index, description = "Zephyr" }, Pref.FallbackSC )

            1 ->
                ( { id = String.fromInt index, description = "#Discord" }
                , Pref.DiscordSC { mainChannelName = "Discord", guildIcon = Just (Image.ph 48 48) }
                )

            _ ->
                ( { id = String.fromInt index, description = "#Slack" }
                , Pref.SlackSC { mainConvName = "Slack", teamIcon = Just (Image.ph 50 50) }
                )
in
Pref.render { onZephyrModeChange = Toggle, onLoggingChange = Toggle }
    { zephyrMode = m.toggle
    , evictThreshold = 5
    , columnSlotsAvailable = m.toggle
    , shadowColumns = List.range 0 6 |> List.map dummyShadowColumn
    , logging = m.toggle
    }""" <|
            let
                dummyShadowColumn index =
                    case modBy 3 index of
                        0 ->
                            ( { id = String.fromInt index, description = "Zephyr" }, Pref.FallbackSC )

                        1 ->
                            ( { id = String.fromInt index, description = "#Discord" }
                            , Pref.DiscordSC { mainChannelName = "Discord", guildIcon = Just (Image.ph 48 48) }
                            )

                        _ ->
                            ( { id = String.fromInt index, description = "#Slack" }
                            , Pref.SlackSC { mainConvName = "Slack", teamIcon = Just (Image.ph 50 50) }
                            )
            in
            Pref.render { onZephyrModeChange = Toggle, onLoggingChange = Toggle }
                { zephyrMode = m.toggle
                , evictThreshold = 5
                , columnSlotsAvailable = m.toggle
                , shadowColumns = List.range 0 6 |> List.map dummyShadowColumn
                , logging = m.toggle
                }
        ]


dummySidebarEffects : Bool -> Sidebar.Effects Msg
dummySidebarEffects isOpen =
    { configOpener = Toggle (not isOpen)
    , columnAdder = AddColumn
    , columnButtonClickerByIndex = always NoOp
    }


mainTemplate : Model -> List (Html Msg)
mainTemplate m =
    View.Template.Main.render
        (mainEffects m)
        (mainProps m)
        { configContents =
            { pref = div [ flexBasis "400px" ] [ t "PREFERENCE[PH]" ]
            , slack = div [ flexBasis "400px" ] [ t "SLACK[PH]" ]
            , discord = div [ flexBasis "400px" ] [ t "DISCORD[PH]" ]
            , status = div [ flexBasis "400px" ] [ t "STATUS[PH]" ]
            }
        , columnContents =
            { header = \index _ -> div [ sizeTitle ] [ t "HEADER[PH] ", t (String.fromInt index) ]
            , config =
                \index _ ->
                    if m.toggle then
                        div [ Border.w1, Border.solid, flexBasis "200px" ] [ t "CONFIG[PH]" ]

                    else
                        none
            , newMessageEditor =
                \_ ->
                    if m.toggle then
                        div [ flexBasis "150px" ] [ t "MESSAGE EDITOR[PH]" ]

                    else
                        none
            , items = \_ -> div [ flexColumn ] <| List.map dummyItem <| List.range 0 10
            }
        }


dummyItem : Int -> Html Msg
dummyItem index =
    case modBy 4 index of
        0 ->
            div [ flexBasis "50px", Background.colorPrim ] [ t "ITEM[PH] ", t (String.fromInt index) ]

        1 ->
            div [ flexBasis "100px", Background.colorSucc ] [ t "ITEM[PH] ", t (String.fromInt index) ]

        2 ->
            div [ flexBasis "200px", Background.colorWarn ] [ t "ITEM[PH] ", t (String.fromInt index) ]

        _ ->
            div [ flexBasis "400px", Background.colorErr ] [ t "ITEM[PH] ", t (String.fromInt index) ]


mainProps : Model -> View.Template.Main.Props ()
mainProps m =
    { sidebarProps = dummySidebarProps m.toggle m.numColumns
    , configDrawerIsOpen = m.toggle
    , columnCtnrProps =
        { visibleColumns = List.repeat m.numColumns ()
        , dragStatus =
            \index _ ->
                case modBy 4 index of
                    0 ->
                        Settled

                    1 ->
                        Undroppable

                    2 ->
                        Droppable

                    _ ->
                        Grabbed
        }
    }


mainEffects : Model -> View.Template.Main.Effects () Msg
mainEffects m =
    { sidebarEffects = dummySidebarEffects m.toggle
    , columnCtnrEffects =
        { columnDragEnd = NoOp
        , columnDragStart = \_ _ -> NoOp
        , columnDragEnter = \_ -> NoOp
        , columnDragOver = NoOp
        }
    }
