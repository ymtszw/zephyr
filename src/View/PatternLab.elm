module View.PatternLab exposing (main)

import AssocList as Dict
import Browser
import Browser.Navigation exposing (Key)
import ColorExtra
import Data.Column exposing (ColumnItem(..))
import Data.ColumnEditor exposing (ColumnEditor(..), UserAction(..))
import Data.Producer.Discord.Guild as DiscordGuild
import Data.Producer.Discord.User as DiscordUser
import File exposing (File)
import File.Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Id
import Json.Decode exposing (succeed)
import List.Extra
import Octicons
import SelectArray
import StringExtra
import Task
import TextParser
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser as U
import View.Atoms.Animation as Animation
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Button as Button
import View.Atoms.Image as Image
import View.Atoms.Input as Input
import View.Atoms.Input.Select as Select
import View.Atoms.Layout exposing (..)
import View.Atoms.Popout as Popout
import View.Atoms.TextBlock exposing (forceBreak, selectAll)
import View.Atoms.Theme exposing (aubergine, oneDark, oneDarkTheme)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column as Column
import View.Molecules.Icon as Icon
import View.Molecules.MarkdownBlocks as MarkdownBlocks
import View.Molecules.MediaViewer as MediaViewer
import View.Molecules.ProducerConfig as ProducerConfig
import View.Molecules.RawColumnItem as RawColumnItem
import View.Molecules.Source exposing (Source(..))
import View.Molecules.Table as Table
import View.Molecules.Wallpaper as Wallpaper
import View.Organisms.Column.Config as ColumnConfig
import View.Organisms.Column.Header as Header
import View.Organisms.Column.Items as Items
import View.Organisms.Column.Items.ItemForView as ItemForView
import View.Organisms.Column.Items.ItemForView.Contents exposing (..)
import View.Organisms.Column.Items.ItemForView.EmbeddedMatter as EmbeddedMatter
import View.Organisms.Column.Items.ItemForView.NamedEntity as NamedEntity
import View.Organisms.Column.NewMessageEditor as NewMessageEditor
import View.Organisms.Config.Discord as Discord
import View.Organisms.Config.Pref as Pref
import View.Organisms.Config.Slack as Slack
import View.Organisms.Config.Status as Status
import View.Organisms.Modeless as Modeless
import View.Organisms.Sidebar as Sidebar
import View.Style exposing (noAttr, none, px)
import View.Stylesheet
import View.Templates.Main exposing (DragStatus(..))


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \m ->
                Sub.batch
                    [ Select.sub SelectCtrl m.select
                    , Sub.map PopoutCtrl (Popout.sub m.popout)
                    , Sub.map ModelessCtrl (Modeless.sub m.modeless)
                    ]
        , onUrlRequest = GoTo
        , onUrlChange = Arrived
        }


type alias Model =
    { key : Key
    , route : Route
    , textInput : String
    , toggle : Bool
    , popout : Popout.State
    , select : Select.State
    , selected : Maybe String
    , numColumns : Int
    , editorSeq : Int
    , editorFile : Maybe ( File, String )
    , userActionOnEditor : UserAction
    , modeless : Modeless.State
    , mediaArray : SelectArray.SelectArray MediaViewer.Media
    , mediaIsShrunk : Bool
    }


type alias Route =
    String


type alias R =
    { path : Route
    , layer : String
    , btnLabel : String
    , view : Model -> List (Html Msg)
    }


routes : List R
routes =
    -- Type-unsafe static route list; introduced in order to reduce repeated pattern matches
    [ R "" "Atoms" "Top" <| pLab [ introduction, theme ]
    , R "typography" "Atoms" "Typography" <| pLab [ typography ]
    , R "text_block" "Atoms" "TextBlock" <| pLab [ textBlock ]
    , R "border" "Atoms" "Border" <| pLab [ border ]
    , R "background" "Atoms" "Background" <| pLab [ background ]
    , R "layout" "Atoms" "Layout" <| pLab [ layout ]
    , R "popout" "Atoms" "Popout" <| \m -> pLab [ popout m ] m
    , R "image" "Atoms" "Image" <| pLab [ image ]
    , R "button" "Atoms" "Button" <| pLab [ button_ ]
    , R "input" "Atoms" "Input" <| \m -> pLab [ input_ m ] m
    , R "animation" "Atoms" "Animation" <| pLab [ animation ]
    , R "icon" "Molecules" "Icon" <| pLab [ icon ]
    , R "wallpaper" "Molecules" "Wallpaper" <| pLab [ wallpaper ]
    , R "table" "Molecules" "Table" <| pLab [ table_ ]
    , R "markdown_blocks" "Molecules" "MarkdownBlocks" <| pLab [ markdownBlocks ]
    , R "producer_config" "Molecules" "ProducerConfig" <| \m -> pLab [ producerConfig m ] m
    , R "column" "Molecules" "Column" <| pLab [ column ]
    , R "rawColumnItem" "Molecules" "RawColumnItem" <| pLab [ rawColumnItem ]
    , R "mediaViewer" "Molecules" "MediaViewer" <| \m -> pLab [ mediaViewer m ] m
    , R "sidebar" "Organisms" "Sidebar" <| \m -> pLab [ sidebar m ] m
    , R "modeless" "Organisms" "Modeless" <| \m -> pLab [ modeless m ] m
    , R "config_pref" "Organisms" "Config.Pref" <| \m -> pLab [ configPref m ] m
    , R "config_status" "Organisms" "Config.Status" <| \m -> pLab [ configStatus m ] m
    , R "config_discord" "Organisms" "Config.Discord" <| \m -> pLab [ configDiscord m ] m
    , R "config_slack" "Organisms" "Config.Slack" <| \m -> pLab [ configSlack m ] m
    , R "column_header" "Organisms" "Column.Header" <| \m -> pLab [ columnHeader m ] m
    , R "column_config" "Organisms" "Column.Config" <| \m -> pLab [ columnConfig m ] m
    , R "column_new_message_editor" "Organisms" "Column.NewMessageEditor" <| \m -> pLab [ columnNewMessageEditor m ] m
    , R "column_items" "Organisms" "Column.Items" <| pLab [ columnItems ]
    , R "main_template" "Templates" "Main" <| mainTemplate
    ]


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , route = urlToRoute url
      , textInput = ""
      , toggle = False
      , popout = Popout.init
      , select = Select.init
      , selected = Nothing
      , numColumns = 4
      , editorSeq = 0
      , editorFile = Nothing
      , userActionOnEditor = OutOfFocus
      , modeless = Modeless.update (Modeless.Touch (Modeless.RawColumnItemId (Id.from "dummy") 0)) Modeless.init
      , mediaArray =
            SelectArray.fromLists
                [ MediaViewer.Image (Image.ph 500 500) ]
                (MediaViewer.Image (Image.ph 300 800))
                [ MediaViewer.Image (Image.ph 800 300)
                , MediaViewer.Video "https://archive.org/download/BigBuckBunny_124/Content/big_buck_bunny_720p_surround.mp4"
                , MediaViewer.Youtube "F9vhni6eNR8"
                , MediaViewer.TwitchChannel "followgrubby"
                ]
      , mediaIsShrunk = False
      }
    , Cmd.none
    )


urlToRoute : Url -> Route
urlToRoute url =
    let
        urlParser =
            U.map matchFirstPath U.string

        matchFirstPath firstPath =
            case List.Extra.find (\r -> r.path == firstPath) routes of
                Just _ ->
                    firstPath

                Nothing ->
                    ""
    in
    Maybe.withDefault "" (U.parse urlParser url)


type Msg
    = NoOp
    | GoTo Browser.UrlRequest
    | Arrived Url
    | TextInput String
    | Toggle Bool
    | PopoutCtrl Popout.Msg
    | SelectCtrl (Select.Msg Msg)
    | Selected String
    | AddColumn
    | EditorInteracted UserAction
    | EditorReset
    | EditorFileRequest (List String)
    | EditorFileSelected File
    | EditorFileLoaded ( File, String )
    | EditorFileDiscard
    | ModelessCtrl Modeless.Msg
    | MediaSelectAt Int
    | MediaToggleSize Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            let
                _ =
                    Debug.log "NoOp" m
            in
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

        PopoutCtrl pMsg ->
            let
                ( newPopout, cmd ) =
                    Popout.update pMsg m.popout
            in
            ( { m | popout = newPopout }, Cmd.map PopoutCtrl cmd )

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

        EditorInteracted action ->
            ( { m | userActionOnEditor = action }, Cmd.none )

        EditorReset ->
            ( { m | textInput = "", editorSeq = m.editorSeq + 1, editorFile = Nothing }, Cmd.none )

        EditorFileRequest mimeTypes ->
            ( { m | userActionOnEditor = Authoring }, File.Select.file mimeTypes EditorFileSelected )

        EditorFileSelected file ->
            ( m, Task.perform (\dataUrl -> EditorFileLoaded ( file, dataUrl )) (File.toUrl file) )

        EditorFileLoaded fileWithDataUrl ->
            ( { m | editorFile = Just fileWithDataUrl }, Cmd.none )

        EditorFileDiscard ->
            ( { m | editorFile = Nothing }, Cmd.none )

        ModelessCtrl mMsg ->
            ( { m | modeless = Modeless.update mMsg m.modeless }, Cmd.none )

        MediaSelectAt index ->
            ( { m | mediaArray = SelectArray.selectAt index m.mediaArray }, Cmd.none )

        MediaToggleSize isShrunk ->
            ( { m | mediaIsShrunk = isShrunk }, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view m =
    { title = "Zephyr: Pattern Lab"
    , body =
        case List.Extra.find (\r -> r.path == m.route) routes of
            Just r ->
                View.Stylesheet.render :: r.view m

            Nothing ->
                []
    }


pLab : List (Html Msg) -> Model -> List (Html Msg)
pLab contents m =
    [ div [ flexColumn, widthFill, spacingColumn15, oneDark ] (navi m.route :: contents) ]


navi : Route -> Html Msg
navi current =
    div [ flexColumn, flexCenter, spacingColumn10, padding15 ] <|
        List.map (naviRow current) <|
            List.Extra.groupWhile (\r1 r2 -> r1.layer == r2.layer) routes


naviRow : Route -> ( R, List R ) -> Html Msg
naviRow current ( r, rs ) =
    div [ flexRow, flexCenter, spacingRow15 ]
        [ h2 [ prominent, bold ] [ t r.layer ]
        , div [ flexRow, flexGrow, flexWrap, flexCenter, spacingWrapped10 ] <|
            List.map (naviButton current) (r :: rs)
        ]


naviButton : Route -> R -> Html Msg
naviButton current r =
    Button.link
        [ prominent
        , padding10
        , flexItem
        , if current == r.path then
            Background.colorSucc

          else
            Background.colorPrim
        ]
        { url = Url.Builder.absolute [ r.path ] []
        , children = [ t r.btnLabel ]
        }


introduction : Html Msg
introduction =
    section []
        [ h1 [ impactful ]
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
            [ h2 [ prominent ]
                [ t "Stylesheet length: "
                , t (StringExtra.punctuateNumber View.Stylesheet.length)
                ]
            ]
        ]


sourceBlock : String -> Html Msg
sourceBlock source =
    let
        sourceLineCount =
            List.length (String.split "\n" source)
    in
    textarea
        [ monospace
        , widthFill
        , padding5
        , Border.round5
        , Background.colorSub
        , readonly True
        , rows sourceLineCount
        , wrap "off"
        ]
        [ t source ]


section : List (Attribute Msg) -> List (Html Msg) -> Html Msg
section attrs =
    div ([ flexColumn, widthFill, flexCenter, spacingColumn15, padding10 ] ++ attrs)


theme : Html Msg
theme =
    section []
        [ h1 [ xxProminent ] [ t "Theme" ]
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


withSource : String -> Html Msg -> Html Msg
withSource source toRender =
    div [ growRow, flexCenter, widthFill, spacingRow15 ]
        [ div [] [ toRender ]
        , sourceBlock source
        ]


typography : Html Msg
typography =
    section []
        [ h1 [ xxProminent ] [ t "Typography" ]
        , fontFamilies
        , fontSizes
        , fontDecorations
        , fontColors
        ]


fontFamilies : Html Msg
fontFamilies =
    section []
        [ h2 [ xProminent ] [ t "Font families" ]
        , withSource "p [ sansSerif ] [ t \"This paragraph uses a sans-serif font. あいうえお水兵リーベ\" ]" <|
            p [ sansSerif ] [ t "This paragraph uses a sans-serif font. あいうえお水兵リーベ" ]
        , withSource "p [ serif ] [ t \"This paragraph uses a serif font. あいうえお水兵リーベ\" ]" <|
            p [ serif ] [ t "This paragraph uses a serif font. あいうえお水兵リーベ" ]
        , withSource "p [ monospace ] [ t \"This paragraph uses a monospace font. あいうえお水兵リーベ\" ]" <|
            p [ monospace ] [ t "This paragraph uses a monospace font. あいうえお水兵リーベ" ]
        ]


fontSizes : Html Msg
fontSizes =
    section []
        [ h2 [ xProminent ] [ t "Font sizes" ]
        , withSource "p [ impactful ] [ t \"impactful\" ]" <|
            p [ impactful ] [ t "impactful" ]
        , withSource "p [ xxProminent ] [ t \"xxProminent\" ]" <|
            p [ xxProminent ] [ t "xxProminent" ]
        , withSource "p [ xProminent ] [ t \"xProminent\" ]" <|
            p [ xProminent ] [ t "xProminent" ]
        , withSource "p [ prominent ] [ t \"prominent\" ]" <|
            p [ prominent ] [ t "prominent" ]
        , withSource "p [ regular ] [ t \"regular\" ]" <|
            p [ regular ] [ t "regular" ]
        , withSource "p [ minuscule ] [ t \"minuscule\" ]" <|
            p [ minuscule ] [ t "minuscule" ]
        ]


fontDecorations : Html Msg
fontDecorations =
    section []
        [ h2 [ xProminent ] [ t "Font decorations" ]
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
                [ h3 [ prominent ] [ t themeText ]
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
        [ h2 [ xProminent ] [ t "Font colors" ]
        , coloredTexts oneDark "oneDark"
        , coloredTexts aubergine "aubergine"
        ]


textBlock : Html Msg
textBlock =
    section []
        [ h1 [ xxProminent ] [ t "Text Blocks" ]
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
        , withSource """p [ selectAll ]
    [ t "Text block with "
    , code [] [ t "selectAll" ]
    , t " attribute will cause user clicks to select all texts in it."
    ]""" <|
            p [ selectAll ]
                [ t "Text block with "
                , code [] [ t "selectAll" ]
                , t " attribute will cause user clicks to select all texts in it."
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
        [ h1 [ xxProminent ] [ t "Border" ]
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
                [ h2 [ xProminent ] [ t themeText ]
                , withSource """div [ padding15 ] [ h3 [ prominent ] [ t "Default" ] ]""" <|
                    div [ padding15 ] [ h3 [ prominent ] [ t "Default" ] ]
                , withSource """div [ padding15, Background.colorBg ] [ h3 [ prominent ] [ t "colorBg" ] ]""" <|
                    div [ padding15, Background.colorBg ] [ h3 [ prominent ] [ t "colorBg" ] ]
                , withSource """div [ padding15, Background.colorMain ] [ h3 [ prominent ] [ t "colorMain" ] ]""" <|
                    div [ padding15, Background.colorMain ] [ h3 [ prominent ] [ t "colorMain" ] ]
                , withSource """div [ padding15, Background.colorSub ] [ h3 [ prominent ] [ t "colorSub" ] ]""" <|
                    div [ padding15, Background.colorSub ] [ h3 [ prominent ] [ t "colorSub" ] ]
                , withSource """div [ padding15, Background.colorNote ] [ h3 [ prominent ] [ t "colorNote" ] ]""" <|
                    div [ padding15, Background.colorNote ] [ h3 [ prominent ] [ t "colorNote" ] ]
                , withSource """div [ padding15, Background.colorPrim ] [ h3 [ prominent ] [ t "colorPrim" ] ]""" <|
                    div [ padding15, Background.colorPrim ] [ h3 [ prominent ] [ t "colorPrim" ] ]
                , withSource """div [ padding15, Background.colorSucc ] [ h3 [ prominent ] [ t "colorSucc" ] ]""" <|
                    div [ padding15, Background.colorSucc ] [ h3 [ prominent ] [ t "colorSucc" ] ]
                , withSource """div [ padding15, Background.colorWarn ] [ h3 [ prominent ] [ t "colorWarn" ] ]""" <|
                    div [ padding15, Background.colorWarn ] [ h3 [ prominent ] [ t "colorWarn" ] ]
                , withSource """div [ padding15, Background.colorErr ] [ h3 [ prominent ] [ t "colorErr" ] ]""" <|
                    div [ padding15, Background.colorErr ] [ h3 [ prominent ] [ t "colorErr" ] ]
                ]
    in
    section []
        [ h1 [ xxProminent ] [ t "Background" ]
        , themedBg oneDark "oneDark"
        , themedBg aubergine "aubergine"
        ]


layout : Html Msg
layout =
    let
        basics =
            section []
                [ h2 [ xProminent ] [ t "Basics" ]
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
        [ h1 [ xxProminent ] [ t "Layout" ]
        , basics
        , flexBox
        , padding
        , spacing
        , badge
        ]


flexBox : Html Msg
flexBox =
    section []
        [ h2 [ xProminent ] [ t "FlexBox" ]
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
        , withSource """div [ flexRow, flexWrap ] <|
    List.repeat 10 <|
        div [ flexBasis (px 100), Border.solid, Border.w1 ] [ t "This is a wrapping row, we are being wrapped!" ]""" <|
            div [ flexRow, flexWrap ] <|
                List.repeat 10 <|
                    div [ flexBasis (px 100), Border.solid, Border.w1 ] [ t "This is a wrapping row, we are being wrapped!" ]
        ]


padding : Html Msg
padding =
    section []
        [ h2 [ xProminent ] [ t "Padding" ]
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
        [ h2 [ xProminent ] [ t "Spacing" ]
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
        , withSource """div [ flexRow, flexWrap, spacingWrapped5, Border.solid, Border.w1 ] <|
    List.repeat 10 <|
        div [ flexBasis (px 100), Border.solid, Border.w1 ] [ t "In a wrapping row, spacing implementation slightly differs!" ]""" <|
            div [ flexRow, flexWrap, spacingWrapped5, Border.solid, Border.w1 ] <|
                List.repeat 10 <|
                    div [ flexBasis (px 100), Border.solid, Border.w1 ] [ t "In a wrapping row, spacing implementation slightly differs!" ]
        , withSource """div [ flexRow, flexWrap, spacingWrapped10, Border.solid, Border.w1 ] <|
        List.repeat 10 <|
            div [ flexBasis (px 100), Border.solid, Border.w1 ] [ t "In a wrapping row, spacing implementation slightly differs!" ]""" <|
            div [ flexRow, flexWrap, spacingWrapped10, Border.solid, Border.w1 ] <|
                List.repeat 10 <|
                    div [ flexBasis (px 100), Border.solid, Border.w1 ] [ t "In a wrapping row, spacing implementation slightly differs!" ]
        ]


badge : Html Msg
badge =
    section []
        [ h2 [ xProminent ] [ t "Badge" ]
        , withSource """withBadge []
    { content =
        div [ Background.colorNote, padding10 ] [ t "I'm main content. This is a special layouting helper Atom for badges. ", t lorem ]
    , topRight =
        Just (div [ Background.colorSucc ] [ t "I'm top-right badge!" ])
    , bottomRight =
        Just (div [ Background.colorErr ] [ t "I'm bottom-right badge!" ])
    }""" <|
            withBadge []
                { content =
                    div [ Background.colorNote, padding10 ] [ t "I'm main content. This is a special layouting helper Atom for badges. ", t lorem ]
                , topRight =
                    Just (div [ Background.colorSucc ] [ t "I'm top-right badge!" ])
                , bottomRight =
                    Just (div [ Background.colorErr ] [ t "I'm bottom-right badge!" ])
                }
        , withSource """withBadge []
    { content = div [ Background.colorNote, padding10 ] [ t lorem ]
    , topRight = Just (div [ Background.colorSucc ] [ t "Top-right only" ])
    , bottomRight = Nothing
    }""" <|
            withBadge []
                { content = div [ Background.colorNote, padding10 ] [ t lorem ]
                , topRight = Just (div [ Background.colorSucc ] [ t "Top-right only" ])
                , bottomRight = Nothing
                }
        , withSource """withBadge []
    { content = div [ Background.colorNote, padding10 ] [ t lorem ]
    , topRight = Nothing
    , bottomRight = Just (div [ Background.colorErr ] [ t "Bottom-right only" ])
    }""" <|
            withBadge []
                { content = div [ Background.colorNote, padding10 ] [ t lorem ]
                , topRight = Nothing
                , bottomRight = Just (div [ Background.colorErr ] [ t "Bottom-right only" ])
                }
        , withSource """withBadge [ badgeOutset ]
    { content =
        div [ Background.colorNote, padding10 ] [ t "With ", code [] [ t "badgeOutset" ], t ", badges are pushed outward. ", t lorem ]
    , topRight =
        Just (div [ Background.colorSucc ] [ t "I'm top-right badge!" ])
    , bottomRight =
        Just (div [ Background.colorErr ] [ t "I'm bottom-right badge!" ])
    }""" <|
            withBadge [ badgeOutset ]
                { content =
                    div [ Background.colorNote, padding10 ] [ t "With ", code [] [ t "badgeOutset" ], t ", badges are pushed outward. ", t lorem ]
                , topRight =
                    Just (div [ Background.colorSucc ] [ t "I'm top-right badge!" ])
                , bottomRight =
                    Just (div [ Background.colorErr ] [ t "I'm bottom-right badge!" ])
                }
        ]


popout : Model -> Html Msg
popout m =
    section []
        [ h1 [ xxProminent ] [ t "Popout" ]
        , withSource """let
    myTooltip =
        let
            config =
                { id = "popoutElementId001"
                , msgTagger = PopoutCtrl
                , orientation = Popout.anchoredVerticallyTo "anchorElementId001"
                }
        in
        Popout.generate config m.popout <|
            \\_ ->
                Popout.node "div"
                    [ style "width" (px 600), colorSucc, padding10, Border.round5, Background.colorBg ]
                    [ t "I'm one huge tooltip! I'm not contained!!! ", t lorem ]
in
Popout.render myTooltip <|
    \\control ->
        Popout.node "div"
            [ style "width" (px 350)
            , style "height" (px 200)
            , style "overflow" "auto"
            , Border.w1
            , Border.solid
            , on "scroll" (succeed control.hide)
            ]
            [ div [ Border.w1, Border.dotted ] [ t lorem ]
            , div [ Border.w1, Border.dotted ] [ t iroha ]
            , div
                [ id "anchorElementId001"
                , xProminent
                , padding10
                , Border.w1
                , Border.dotted
                , onMouseEnter control.show
                , onMouseLeave control.hide
                ]
                [ t "Hover cursor on me to reveal a tooltip!" ]
            , div [ Border.w1, Border.dotted ] [ t lorem ]
            , div [ Border.w1, Border.dotted ] [ t iroha ]
            ]""" <|
            let
                myTooltip =
                    let
                        config =
                            { id = "popoutElementId001"
                            , msgTagger = PopoutCtrl
                            , orientation = Popout.anchoredVerticallyTo "anchorElementId001"
                            }
                    in
                    Popout.generate config m.popout <|
                        \_ ->
                            Popout.node "div"
                                [ style "width" (px 600), colorSucc, padding10, Border.round5, Background.colorBg ]
                                [ t "I'm one huge tooltip! I'm not contained!!! ", t lorem ]
            in
            Popout.render myTooltip <|
                \control ->
                    Popout.node "div"
                        [ style "width" (px 350)
                        , style "height" (px 200)
                        , style "overflow" "auto"
                        , Border.w1
                        , Border.solid
                        , on "scroll" (succeed control.hide)
                        ]
                        [ div [ Border.w1, Border.dotted ] [ t lorem ]
                        , div [ Border.w1, Border.dotted ] [ t iroha ]
                        , div
                            [ id "anchorElementId001"
                            , xProminent
                            , padding10
                            , Border.w1
                            , Border.dotted
                            , onMouseEnter control.show
                            , onMouseLeave control.hide
                            ]
                            [ t "Hover cursor on me to reveal a tooltip!" ]
                        , div [ Border.w1, Border.dotted ] [ t lorem ]
                        , div [ Border.w1, Border.dotted ] [ t iroha ]
                        ]
        , withSource """let
    myTooltip =
        let
            config =
                { id = "popoutElementId002"
                , msgTagger = PopoutCtrl
                , orientation = Popout.anchoredVerticallyTo "anchorElementId002"
                }
        in
        Popout.generate config m.popout <|
            \\_ ->
                Popout.node "div"
                    [ style "width" (px 600), colorSucc, padding10, Border.round5, Background.colorBg ]
                    [ t "I'm one huge tooltip! I'm not contained!!! ", t lorem ]
in
Popout.render myTooltip <|
    \\control ->
        Popout.node "div"
            [ style "width" (px 350)
            , style "height" (px 100)
            , style "overflow" "auto"
            , Border.w1
            , Border.solid
            , on "scroll" (succeed control.hide)
            ]
            [ div [ Border.w1, Border.dotted ] [ t lorem ]
            , div [ Border.w1, Border.dotted ] [ t iroha ]
            , div
                [ id "anchorElementId002"
                , xProminent
                , padding10
                , Border.w1
                , Border.dotted
                , onClick control.show
                ]
                [ t "Click me to reveal a tooltip!" ]
            , div [ Border.w1, Border.dotted ] [ t lorem ]
            , div [ Border.w1, Border.dotted ] [ t iroha ]
            ]""" <|
            let
                myTooltip =
                    let
                        config =
                            { id = "popoutElementId002"
                            , msgTagger = PopoutCtrl
                            , orientation = Popout.anchoredVerticallyTo "anchorElementId002"
                            }
                    in
                    Popout.generate config m.popout <|
                        \_ ->
                            Popout.node "div"
                                [ style "width" (px 600), colorSucc, padding10, Border.round5, Background.colorBg ]
                                [ t "I'm one huge tooltip! I'm not contained!!! ", t lorem ]
            in
            Popout.render myTooltip <|
                \control ->
                    Popout.node "div"
                        [ style "width" (px 350)
                        , style "height" (px 100)
                        , style "overflow" "auto"
                        , Border.w1
                        , Border.solid
                        , on "scroll" (succeed control.hide)
                        ]
                        [ div [ Border.w1, Border.dotted ] [ t lorem ]
                        , div [ Border.w1, Border.dotted ] [ t iroha ]
                        , div
                            [ id "anchorElementId002"
                            , xProminent
                            , padding10
                            , Border.w1
                            , Border.dotted
                            , onClick control.show
                            ]
                            [ t "Click me to reveal a tooltip!" ]
                        , div [ Border.w1, Border.dotted ] [ t lorem ]
                        , div [ Border.w1, Border.dotted ] [ t iroha ]
                        ]
        ]


image : Html Msg
image =
    section []
        [ h1 [ xxProminent ] [ t "Image" ]
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
    , div [ oneDark ]
        [ Image.octicon { size = 30, shape = Octicons.star }
        , span [ Image.fillPrim ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillSucc ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillWarn ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillErr ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillText ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        ]
    , div [ aubergine ]
        [ Image.octicon { size = 30, shape = Octicons.star }
        , span [ Image.fillPrim ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillSucc ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillWarn ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillErr ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        , span [ Image.fillText ] [ Image.octicon { size = 30, shape = Octicons.star } ]
        ]
    ]""" <|
            div []
                [ t "Octicon fill colors depend on upstream theme and custom styles. "
                , div [ oneDark ]
                    [ Image.octicon { size = 30, shape = Octicons.star }
                    , span [ Image.fillPrim ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillSucc ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillWarn ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillErr ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillText ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    ]
                , div [ aubergine ]
                    [ Image.octicon { size = 30, shape = Octicons.star }
                    , span [ Image.fillPrim ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillSucc ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillWarn ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillErr ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    , span [ Image.fillText ] [ Image.octicon { size = 30, shape = Octicons.star } ]
                    ]
                ]
        ]


button_ : Html Msg
button_ =
    let
        themedStdButtons theme_ themeText =
            section [ theme_ ]
                [ h2 [ xProminent ] [ t themeText ]
                , withSource """button [] [ t "default" ]""" <| button [] [ t "default" ]
                , withSource """button [ Background.colorPrim ] [ t "prim" ]""" <| button [ Background.colorPrim ] [ t "prim" ]
                , withSource """button [ Background.colorSucc ] [ t "succ" ]""" <| button [ Background.colorSucc ] [ t "succ" ]
                , withSource """button [ Background.colorWarn ] [ t "warn" ]""" <| button [ Background.colorWarn ] [ t "warn" ]
                , withSource """button [ Background.colorErr ] [ t "err" ]""" <| button [ Background.colorErr ] [ t "err" ]
                , withSource """button [ Background.colorPrim, prominent ] [ t "prim" ]""" <| button [ Background.colorPrim, prominent ] [ t "prim prominent" ]
                , withSource """button [ Background.colorPrim, xxProminent ] [ t "prim" ]""" <| button [ Background.colorPrim, xxProminent ] [ t "prim SECTION" ]
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
                , withSource """div [ growRow, prominent ]
    [ button [ Background.colorSucc, Border.leftRound5 ] [ t "We can achieve coupled button row" ]
    , button [ Background.colorWarn, Border.noRound ] [ t "We can achieve coupled button row" ]
    , button [ Background.colorErr, Border.rightRound5 ] [ t "We can achieve coupled button row" ]
    ]""" <|
                    div [ growRow, prominent ]
                        [ button [ Background.colorSucc, Border.leftRound5 ] [ t "We can achieve coupled button row" ]
                        , button [ Background.colorWarn, Border.noRound ] [ t "We can achieve coupled button row" ]
                        , button [ Background.colorErr, Border.rightRound5 ] [ t "We can achieve coupled button row" ]
                        ]
                ]

        themedLinkButtons theme_ themeText =
            section [ theme_ ]
                [ h2 [ xProminent ] [ t themeText ]
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
                , withSource """Button.link [ Background.colorPrim, padding10, xxProminent, Border.w1, Border.solid ]
    { url = "https://example.com"
    , children = [ t "Can be padded/sized/bordered" ]
    }""" <|
                    Button.link [ Background.colorPrim, padding10, xxProminent, Border.w1, Border.solid ]
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
        [ h1 [ xxProminent ] [ t "Button" ]
        , themedStdButtons oneDark "oneDark"
        , themedStdButtons aubergine "aubergine"
        , h1 [ xxProminent ] [ t "Link Button" ]
        , themedLinkButtons oneDark "oneDark"
        , themedLinkButtons aubergine "aubergine"
        ]


input_ : Model -> Html Msg
input_ m =
    section []
        [ h1 [ xxProminent ] [ t "Input" ]
        , textInput m.textInput
        , toggle m.toggle
        , select_ m.select m.selected
        ]


textInput : String -> Html Msg
textInput currentInput =
    section []
        [ h2 [ xProminent ] [ t "Text" ]
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
        , prominent
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
                    , prominent
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
        [ h2 [ xProminent ] [ t "Toggle" ]
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
        [ h2 [ xProminent ] [ t "Select" ]
        , withSource """div []
    [ t "By default these are block elements."
    , Select.render []
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
                , Select.render []
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
    , Select.render [ style "max-width" "300px" ]
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
                , Select.render [ style "max-width" "300px" ]
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
    , Select.render []
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
                , Select.render []
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
    , Select.render []
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
                , Select.render []
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
    , Select.render [ aubergine ]
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
                , Select.render [ aubergine ]
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


animation : Html Msg
animation =
    section []
        [ h1 [ xxProminent ] [ t "Animation" ]
        , withSource """img [ Animation.rotating, src (Image.ph 50 50), alt "50x50 image" ] []""" <|
            img [ Animation.rotating, src (Image.ph 50 50), alt "50x50 image" ] []
        , withSource """img [ Animation.slideDown, src (Image.ph 50 50), alt "50x50 image" ] []""" <|
            img [ Animation.slideDown, src (Image.ph 50 50), alt "50x50 image" ] []
        , withSource """img [ Border.w1, Border.solid, Animation.borderFlash, src (Image.ph 50 50), alt "50x50 image" ] []""" <|
            img [ Border.w1, Border.solid, Animation.borderFlash, src (Image.ph 50 50), alt "50x50 image" ] []
        ]


icon : Html Msg
icon =
    section []
        [ h1 [ xxProminent ] [ t "Icon" ]
        , withSource """Icon.button [] { onPress = NoOp, src = Image.ph 50 50, alt = "50x50 icon" }""" <|
            Icon.button [] { onPress = NoOp, src = Image.ph 50 50, alt = "50x50 icon" }
        , withSource """Icon.button [ Border.round5 ] { onPress = NoOp, src = Image.ph 75 75, alt = "75x75 icon" }""" <|
            Icon.button [ Border.round5 ] { onPress = NoOp, src = Image.ph 75 75, alt = "75x75 icon" }
        , withSource """Icon.link [] { url = "https://example.com", src = Image.ph 50 50, alt = "50x50 icon" }""" <|
            Icon.link [] { url = "https://example.com", src = Image.ph 50 50, alt = "50x50 icon" }
        , withSource """Icon.link [ Border.round5, newTab ] { url = "https://example.com", src = Image.ph 75 75, alt = "75x75 icon" }""" <|
            Icon.link [ Border.round5, newTab ] { url = "https://example.com", src = Image.ph 75 75, alt = "75x75 icon" }
        , withSource """Icon.abbr [ style "width" "50px", style "height" "50px" ] "Zephyr\"""" <|
            Icon.abbr [ style "width" "50px", style "height" "50px" ] "Zephyr"
        , withSource """Icon.abbr [ style "width" "60px", style "height" "80px", xProminent, Border.round5 ] "Zephyr\"""" <|
            Icon.abbr [ style "width" "60px", style "height" "80px", xProminent, Border.round5 ] "Zephyr"
        , withSource """Icon.imgOrAbbr [ style "width" "50px", style "height" "50px" ] "Zephyr" (Just (Image.ph 50 50))""" <|
            Icon.imgOrAbbr [ style "width" "50px", style "height" "50px" ] "Zephyr" (Just (Image.ph 50 50))
        , withSource """Icon.imgOrAbbr [ style "width" "50px", style "height" "50px" ] "Zephyr" Nothing""" <|
            Icon.imgOrAbbr [ style "width" "50px", style "height" "50px" ] "Zephyr" Nothing
        , withSource """Icon.octiconBlock [ style "width" "75px", style "height" "75px" ] { size = 50, shape = Octicons.search }""" <|
            Icon.octiconBlock [ style "width" "75px", style "height" "75px" ] { size = 50, shape = Octicons.search }
        , withSource """Icon.octiconButton [] { size = 50, onPress = NoOp, shape = Octicons.search }""" <|
            Icon.octiconButton [] { size = 50, onPress = NoOp, shape = Octicons.search }
        , withSource """Icon.octiconButton [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, onPress = NoOp, shape = Octicons.search }""" <|
            Icon.octiconButton [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, onPress = NoOp, shape = Octicons.search }
        , withSource """Icon.octiconLink [] { size = 50, url = "https://example.com", shape = Octicons.rocket }""" <|
            Icon.octiconLink [] { size = 50, url = "https://example.com", shape = Octicons.rocket }
        , withSource """Icon.octiconLink [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, url = "https://example.com", shape = Octicons.rocket }""" <|
            Icon.octiconLink [ padding5, Background.colorSucc, Border.round5, newTab ] { size = 75, url = "https://example.com", shape = Octicons.rocket }
        ]


wallpaper : Html Msg
wallpaper =
    section []
        [ h1 [ xxProminent ] [ t "Wallpaper" ]
        , withSource "Wallpaper.zephyr" Wallpaper.zephyr
        ]


table_ : Html Msg
table_ =
    section []
        [ h1 [ xxProminent ] [ t "Table" ]
        , withSource """Table.render []
    { columns =
        [ { header = "Empty Table Column1", cell = always ( [], [] ) }
        , { header = "Empty Table Column2", cell = always ( [], [] ) }
        ]
    , rowKey = identity
    , data = []
    }""" <|
            Table.render []
                { columns =
                    [ { header = "Empty Table Column1", cell = always ( [], [] ) }
                    , { header = "Empty Table Column2", cell = always ( [], [] ) }
                    ]
                , rowKey = identity
                , data = []
                }
        , withSource """Table.render []
    { columns =
        [ { header = "Number"
          , cell = \\i -> ( [], [ t (String.fromInt i) ] )
          }
        , { header = "Multiplied and Punctuated"
          , cell = \\i -> ( [], [ t (StringExtra.punctuateNumber (i * 999)) ] )
          }
        , { header = "Multiplied, Punctuated and Reversed"
          , cell = \\i -> ( [], [ t (String.reverse (StringExtra.punctuateNumber (i * 999))) ] )
          }
        ]
    , rowKey = String.fromInt
    , data = List.range 0 10
    }""" <|
            Table.render []
                { columns =
                    [ { header = "Number"
                      , cell = \i -> ( [], [ t (String.fromInt i) ] )
                      }
                    , { header = "Multiplied and Punctuated"
                      , cell = \i -> ( [], [ t (StringExtra.punctuateNumber (i * 999)) ] )
                      }
                    , { header = "Multiplied, Punctuated and Reversed"
                      , cell = \i -> ( [], [ t (String.reverse (StringExtra.punctuateNumber (i * 999))) ] )
                      }
                    ]
                , rowKey = String.fromInt
                , data = List.range 0 10
                }
        , withSource """Table.render []
    { columns =
        [ { header = "Can set attributes to cell (though widthFill only works for a single column)"
          , cell = \\i -> ( [ widthFill ], [ t (String.fromInt i) ] )
          }
        , { header = "Multiplied and Punctuated"
          , cell = \\i -> ( [], [ t (StringExtra.punctuateNumber (i * 999)) ] )
          }
        , { header = "Multiplied, Punctuated and Reversed"
          , cell = \\i -> ( [], [ t (String.reverse (StringExtra.punctuateNumber (i * 999))) ] )
          }
        ]
    , rowKey = String.fromInt
    , data = List.range 0 10
    }""" <|
            Table.render []
                { columns =
                    [ { header = "Can set attributes to cell (though widthFill only works for a single column)"
                      , cell = \i -> ( [ widthFill ], [ t (String.fromInt i) ] )
                      }
                    , { header = "Multiplied and Punctuated"
                      , cell = \i -> ( [], [ t (StringExtra.punctuateNumber (i * 999)) ] )
                      }
                    , { header = "Multiplied, Punctuated and Reversed"
                      , cell = \i -> ( [], [ t (String.reverse (StringExtra.punctuateNumber (i * 999))) ] )
                      }
                    ]
                , rowKey = String.fromInt
                , data = List.range 0 10
                }
        , withSource """Table.render []
    { columns =
        [ { header = "Image"
          , cell = \\( size, src_ ) -> ( [], [ img [ src src_, alt (String.fromInt size ++ "px image") ] [] ] )
          }
        , { header = "Size"
          , cell = \\( size, _ ) -> ( [], [ t (px size) ] )
          }
        , { header = "Source URL"
          , cell = \\( _, src_ ) -> ( [], [ link [] { url = src_, children = [ t src_ ] } ] )
          }
        , { header = "Description"
          , cell = \\_ -> ( [], [ p [] [ t iroha, t " ", t lorem ] ] )
          }
        ]
    , rowKey = \\( size, _ ) -> "imageSize_" ++ String.fromInt size
    , data = [ ( 50, Image.ph 50 50 ), ( 100, Image.ph 100 100 ), ( 300, Image.ph 300 300 ) ]
    }""" <|
            Table.render []
                { columns =
                    [ { header = "Image"
                      , cell = \( size, src_ ) -> ( [], [ img [ src src_, alt (String.fromInt size ++ "px image") ] [] ] )
                      }
                    , { header = "Size"
                      , cell = \( size, _ ) -> ( [], [ t (px size) ] )
                      }
                    , { header = "Source URL"
                      , cell = \( _, src_ ) -> ( [], [ link [] { url = src_, children = [ t src_ ] } ] )
                      }
                    , { header = "Description"
                      , cell = \_ -> ( [], [ p [] [ t iroha, t " ", t lorem ] ] )
                      }
                    ]
                , rowKey = \( size, _ ) -> "imageSize_" ++ String.fromInt size
                , data = [ ( 50, Image.ph 50 50 ), ( 100, Image.ph 100 100 ), ( 300, Image.ph 300 300 ) ]
                }
        , withSource """Table.render [ Table.layoutFixed ]
    { columns =
        [ { header = "Image"
          , cell = \\( size, src_ ) -> ( [], [ img [ src src_, alt (String.fromInt size ++ "px image") ] [] ] )
          }
        , { header = "Size"
          , cell = \\( size, _ ) -> ( [], [ t (px size) ] )
          }
        , { header = "Source URL"
          , cell = \\( _, src_ ) -> ( [], [ link [] { url = src_, children = [ t src_ ] } ] )
          }
        , { header = "Description"
          , cell = \\_ -> ( [], [ p [] [ t iroha, t " ", t lorem ] ] )
          }
        ]
    , rowKey = \\( size, _ ) -> "imageSize_" ++ String.fromInt size
    , data = [ ( 50, Image.ph 50 50 ), ( 100, Image.ph 100 100 ), ( 300, Image.ph 300 300 ) ]
    }""" <|
            Table.render [ Table.layoutFixed ]
                { columns =
                    [ { header = "Image"
                      , cell = \( size, src_ ) -> ( [], [ img [ src src_, alt (String.fromInt size ++ "px image") ] [] ] )
                      }
                    , { header = "Size"
                      , cell = \( size, _ ) -> ( [], [ t (px size) ] )
                      }
                    , { header = "Source URL"
                      , cell = \( _, src_ ) -> ( [], [ link [] { url = src_, children = [ t src_ ] } ] )
                      }
                    , { header = "Description"
                      , cell = \_ -> ( [], [ p [] [ t iroha, t " ", t lorem ] ] )
                      }
                    ]
                , rowKey = \( size, _ ) -> "imageSize_" ++ String.fromInt size
                , data = [ ( 50, Image.ph 50 50 ), ( 100, Image.ph 100 100 ), ( 300, Image.ph 300 300 ) ]
                }
        ]


markdownBlocks : Html Msg
markdownBlocks =
    let
        themed theme_ themeStr =
            section [ theme_ ]
                [ h2 [ xProminent ] [ t themeStr ]
                , col 400 "MarkdownBlocks.render TextParser.defaultOptions MarkdownBlocks.sampleSource" <|
                    MarkdownBlocks.render TextParser.defaultOptions MarkdownBlocks.sampleSource
                , col 100 """MarkdownBlocks.render TextParser.defaultOptions <|
    "What if there are significantly long strings?\\n\\n"
        ++ String.repeat 100 "abcd0123\"""" <|
                    MarkdownBlocks.render TextParser.defaultOptions <|
                        "What if there are significantly long strings?\n\n"
                            ++ String.repeat 100 "abcd0123"
                ]

        col height_ source_ contents =
            withSource source_ <|
                div []
                    [ t "(Contained)"
                    , div
                        [ style "width" (px 350)
                        , style "height" (px height_)
                        , style "overflow" "auto"
                        , flexColumn
                        , spacingColumn2
                        , Border.w1
                        , Border.solid
                        , Border.colorBg
                        ]
                        contents
                    ]
    in
    section []
        [ h1 [ xxProminent ] [ t "MarkdownBlocks" ]
        , themed oneDark "oneDark"
        , themed aubergine "aubergine"
        , section []
            [ h2 [ xProminent ] [ t "Sample Markdown" ]
            , sourceBlock MarkdownBlocks.sampleSource
            ]
        ]


producerConfig : Model -> Html Msg
producerConfig m =
    section []
        [ h1 [ xxProminent ] [ t "ProducerConfig" ]
        , withSource """ProducerConfig.tokenForm
    { onInput = TextInput
    , onSubmit = NoOp
    }
    { id = "tokenFormId"
    , token = m.textInput
    , submittable = True
    , submitButtonText = "Register"
    , apiDomain = "api.example.com"
    }""" <|
            ProducerConfig.tokenForm
                { onInput = TextInput
                , onSubmit = NoOp
                }
                { id = "tokenFormId"
                , token = m.textInput
                , submittable = True
                , submitButtonText = "Register"
                , apiDomain = "api.example.com"
                }
        , withSource """ProducerConfig.subSelect
    { onSelect = always NoOp
    , selectMsgTagger = SelectCtrl
    }
    { id = "subSelectId"
    , selectState = m.select
    , options =
        [ { id = Id.from "Subbable1", name = "Name1" }
        , { id = Id.from "Subbable2", name = String.repeat 3 "Name2" }
        ]
    , filterMatch = \\query e -> String.contains query e.name
    , optionHtml = \\e -> t e.name
    }""" <|
            ProducerConfig.subSelect
                { onSelect = always NoOp
                , selectMsgTagger = SelectCtrl
                }
                { id = "subSelectId"
                , selectState = m.select
                , options =
                    [ { id = Id.from "Subbable1", name = "Name1" }
                    , { id = Id.from "Subbable2", name = String.repeat 3 "Name2" }
                    ]
                , filterMatch = \query e -> String.contains query e.name
                , optionHtml = \e -> t e.name
                }
        , withSource """ProducerConfig.subbedTable
    { onCreateColumnButtonClick = always NoOp
    , onForceFetchButtonClick = always (Toggle (not m.toggle))
    , onUnsubscribeButtonClick = always NoOp
    }
    { items =
        [ { id = Id.from "ID1", name = "Name1", fetching = m.toggle, producing = m.toggle }
        , { id = Id.from "ID2", name = String.repeat 3 "Name3", fetching = m.toggle, producing = m.toggle }
        ]
    , itemHtml = \\i -> t i.name
    }""" <|
            ProducerConfig.subbedTable
                { onCreateColumnButtonClick = always NoOp
                , onForceFetchButtonClick = always (Toggle (not m.toggle))
                , onUnsubscribeButtonClick = always NoOp
                }
                { items =
                    [ { id = Id.from "ID1", name = "Name1", fetching = m.toggle, producing = m.toggle }
                    , { id = Id.from "ID2", name = String.repeat 3 "Name3", fetching = m.toggle, producing = m.toggle }
                    ]
                , itemHtml = \i -> t i.name
                }
        ]


column : Html Msg
column =
    section []
        [ h1 [ xxProminent ] [ t "Column" ]
        , section []
            [ h2 [ xProminent ] [ t "inlineTitle" ]
            , withSource """div [ prominent ] <|
    Column.inlineTitle 15 <|
        { pinned = False, sources = [], filters = [] }""" <|
                div [ prominent ] <|
                    Column.inlineTitle 15 <|
                        { pinned = False, sources = [], filters = [] }
            , withSource """div [ prominent ] <|
    Column.inlineTitle 15 <|
        { pinned = False, sources = [], filters = [ "\\"Elm\\"", "Has Media" ] }""" <|
                div [ prominent ] <|
                    Column.inlineTitle 15 <|
                        { pinned = False, sources = [], filters = [ "\"Elm\"", "Has Media" ] }
            , withSource """div [ prominent ] <|
    Column.inlineTitle 15 <|
        { pinned = False
        , sources = [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing } ]
        , filters = []
        }""" <|
                div [ prominent ] <|
                    Column.inlineTitle 15 <|
                        { pinned = False
                        , sources = [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing } ]
                        , filters = []
                        }
            , withSource """div [ prominent ] <|
    Column.inlineTitle 15 <|
        { pinned = False
        , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = False } ]
        , filters = []
        }""" <|
                div [ prominent ] <|
                    Column.inlineTitle 15 <|
                        { pinned = False
                        , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = False } ]
                        , filters = []
                        }
            , withSource """div [ prominent ] <|
    Column.inlineTitle 15 <|
        { pinned = False
        , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True } ]
        , filters = []
        }""" <|
                div [ prominent ] <|
                    Column.inlineTitle 15 <|
                        { pinned = False
                        , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True } ]
                        , filters = []
                        }
            , withSource """div [ prominent ] <|
    Column.inlineTitle 15 <|
        { pinned = False
        , sources =
            [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing }
            , SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True }
            ]
        , filters = [ "\\"Elm\\"", "Has Media" ]
        }""" <|
                div [ prominent ] <|
                    Column.inlineTitle 15 <|
                        { pinned = False
                        , sources =
                            [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing }
                            , SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True }
                            ]
                        , filters = [ "\"Elm\"", "Has Media" ]
                        }
            ]
        , section []
            [ h2 [ xProminent ] [ t "blockTitle" ]
            , withSource """Column.blockTitle [] <|
    { pinned = False, sources = [], filters = [] }""" <|
                Column.blockTitle [] <|
                    { pinned = False, sources = [], filters = [] }
            , withSource """Column.blockTitle [] <|
    { pinned = False, sources = [], filters = [ "\\"Elm\\"", "Has Media" ] }""" <|
                Column.blockTitle [] <|
                    { pinned = False, sources = [], filters = [ "\"Elm\"", "Has Media" ] }
            , withSource """Column.blockTitle [] <|
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing } ]
    , filters = []
    }""" <|
                Column.blockTitle [] <|
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing } ]
                    , filters = []
                    }
            , withSource """Column.blockTitle [] <|
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = False } ]
    , filters = []
    }""" <|
                Column.blockTitle [] <|
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.blockTitle [] <|
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True } ]
    , filters = []
    }""" <|
                Column.blockTitle [] <|
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True } ]
                    , filters = []
                    }
            , withSource """Column.blockTitle [] <|
    { pinned = False
    , sources =
        [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing }
        , SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True }
        ]
    , filters = [ "\\"Elm\\"", "Has Media" ]
    }""" <|
                Column.blockTitle [] <|
                    { pinned = False
                    , sources =
                        [ DiscordSource { id = "DID", name = "Channel", guildName = "Guild", guildIcon = Nothing }
                        , SlackSource { id = "SID", name = "Convo", teamName = "Team", teamIcon = Nothing, isPrivate = True }
                        ]
                    , filters = [ "\"Elm\"", "Has Media" ]
                    }
            , withSource """Column.blockTitle [ style "width" (px 350) ] <|
    { pinned = False
    , sources =
        [ DiscordSource { id = "DID", name = String.repeat 3 "Clipped if constrained ", guildName = "Guild", guildIcon = Nothing }
        ]
    , filters = List.repeat 5 "Clipped if constrained "
    }""" <|
                Column.blockTitle [ style "width" (px 350) ] <|
                    { pinned = False
                    , sources =
                        [ DiscordSource { id = "DID", name = String.repeat 3 "Clipped if constrained ", guildName = "Guild", guildIcon = Nothing }
                        ]
                    , filters = List.repeat 5 "Clipped if constrained "
                    }
            ]
        , section []
            [ h2 [ xProminent ] [ t "icons" ]
            , withSource """Column.icon20 { pinned = False, sources = [], filters = [] }""" <|
                Column.icon20 { pinned = False, sources = [], filters = [] }
            , withSource """Column.icon20
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
    , filters = []
    }""" <|
                Column.icon20
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
                    , filters = []
                    }
            , withSource """Column.icon20
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
    , filters = []
    }""" <|
                Column.icon20
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
                    , filters = []
                    }
            , withSource """Column.icon20
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon20
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon20
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon20
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon30 { pinned = False, sources = [], filters = [] }""" <|
                Column.icon30 { pinned = False, sources = [], filters = [] }
            , withSource """Column.icon30
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
    , filters = []
    }""" <|
                Column.icon30
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
                    , filters = []
                    }
            , withSource """Column.icon30
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
    , filters = []
    }""" <|
                Column.icon30
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
                    , filters = []
                    }
            , withSource """Column.icon30
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon30
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon30
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon30
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon40 { pinned = False, sources = [], filters = [] }""" <|
                Column.icon40 { pinned = False, sources = [], filters = [] }
            , withSource """Column.icon40 { pinned = True, sources = [], filters = [] }""" <|
                Column.icon40 { pinned = True, sources = [], filters = [] }
            , withSource """Column.icon40
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = True
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = True
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Nothing } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = False
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = False
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = True
    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = True
                    , sources = [ DiscordSource { id = "DID", name = "D", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = True
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = True
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Nothing, isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = False
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = False
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
                    , filters = []
                    }
            , withSource """Column.icon40
    { pinned = True
    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
    , filters = []
    }""" <|
                Column.icon40
                    { pinned = True
                    , sources = [ SlackSource { id = "SID", name = "S", teamName = "チーム", teamIcon = Just (Image.ph 21 21), isPrivate = False } ]
                    , filters = []
                    }
            ]
        ]


rawColumnItem : Html Msg
rawColumnItem =
    let
        significantlylongstring =
            String.repeat 50 "significantlylongstring"
    in
    section []
        [ h1 [ xxProminent ] [ t "RawColumnItem" ]
        , withSource """RawColumnItem.render <|
    SystemMessage
        { id = "sm01"
        , message = "System Message " ++ lorem ++ iroha ++ significantlylongstring
        , mediaMaybe = Nothing
        }""" <|
            RawColumnItem.render <|
                SystemMessage
                    { id = "sm01"
                    , message = "System Message " ++ lorem ++ iroha ++ significantlylongstring
                    , mediaMaybe = Nothing
                    }
        , withSource """RawColumnItem.render <|
    SystemMessage
        { id = "sm01"
        , message = "With media"
        , mediaMaybe = Just (Data.Column.Image (StringExtra.toUrlUnsafe "https://example.com/image.png"))
        }""" <|
            RawColumnItem.render <|
                SystemMessage
                    { id = "sm01"
                    , message = "With media"
                    , mediaMaybe = Just (Data.Column.Image (StringExtra.toUrlUnsafe "https://example.com/image.png"))
                    }
        , withSource """RawColumnItem.render <|
    LocalMessage { id = "lm01", message = "Local Message (Personal Memo) " ++ lorem }""" <|
            RawColumnItem.render <|
                LocalMessage { id = "lm01", message = "Local Message (Personal Memo) " ++ lorem }
        ]


mediaViewer : Model -> Html Msg
mediaViewer m =
    section []
        [ h1 [ xxProminent ] [ t "MediaViewer" ]
        , withSource """MediaViewer.render
    { onPagerClick = MediaSelectAt
    , onToggleSizeClick = MediaToggleSize
    }
    { selectedMedia = MediaViewer.NotFound
    , mediaIndex = 0
    , nMedia = 0
    , isShrunk = False
    }""" <|
            MediaViewer.render
                { onPagerClick = MediaSelectAt
                , onToggleSizeClick = MediaToggleSize
                }
                { selectedMedia = MediaViewer.NotFound
                , mediaIndex = 0
                , nMedia = 0
                , isShrunk = False
                }
        , withSource """MediaViewer.render
    { onPagerClick = MediaSelectAt
    , onToggleSizeClick = MediaToggleSize
    }
    { selectedMedia = SelectArray.selected m.mediaArray
    , mediaIndex = SelectArray.selectedIndex m.mediaArray
    , nMedia = SelectArray.size m.mediaArray
    , isShrunk = m.mediaIsShrunk
    }""" <|
            MediaViewer.render
                { onPagerClick = MediaSelectAt
                , onToggleSizeClick = MediaToggleSize
                }
                { selectedMedia = SelectArray.selected m.mediaArray
                , mediaIndex = SelectArray.selectedIndex m.mediaArray
                , nMedia = SelectArray.size m.mediaArray
                , isShrunk = m.mediaIsShrunk
                }
        ]


sidebar : Model -> Html Msg
sidebar m =
    section []
        [ h1 [ xxProminent ] [ t "Sidebar" ]
        , withSource """let
    dummyColumn index =
        { id = Id.from (String.fromInt index)
        , pinned = modBy 2 index == 0
        , sources =
            case modBy 3 index of
                0 ->
                    []

                1 ->
                    [ DiscordSource { id = "DID" ++ String.fromInt index, name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 48 48) } ]

                _ ->
                    [ SlackSource { id = "SID" ++ String.fromInt index, name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 50 50), isPrivate = True }
                    , DiscordSource { id = "DID" ++ String.fromInt index, name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 48 48) }
                    ]
        , filters =
            case modBy 2 index of
                0 ->
                    []

                _ ->
                    [ ""Elm"", "Has Media" ]
        }
in
Sidebar.render
    { onConfigToggleClick = Toggle
    , onAddColumnClick = AddColumn
    , onColumnButtonClickByIndex = always NoOp
    }
    { configOpen = m.toggle
    , visibleColumns = List.map dummyColumn (List.range 0 (m.numColumns - 1))
    }""" <|
            let
                dummyColumn index =
                    { id = Id.from (String.fromInt index)
                    , pinned = modBy 2 index == 0
                    , sources =
                        case modBy 3 index of
                            0 ->
                                []

                            1 ->
                                [ DiscordSource { id = "DID" ++ String.fromInt index, name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 48 48) } ]

                            _ ->
                                [ SlackSource { id = "SID" ++ String.fromInt index, name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 50 50), isPrivate = True }
                                , DiscordSource { id = "DID" ++ String.fromInt index, name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 48 48) }
                                ]
                    , filters =
                        case modBy 2 index of
                            0 ->
                                []

                            _ ->
                                [ "\"Elm\"", "Has Media" ]
                    }
            in
            Sidebar.render
                { onConfigToggleClick = Toggle
                , onAddColumnClick = AddColumn
                , onColumnButtonClickByIndex = always NoOp
                }
                { configOpen = m.toggle
                , visibleColumns = List.map dummyColumn (List.range 0 (m.numColumns - 1))
                }
        ]


modeless : Model -> Html Msg
modeless m =
    section []
        [ h1 [ xxProminent ] [ t "Modeless" ]
        , withSource """div []
    [ div [ flexColumn, spacingColumn10 ]
        [ button [ flexItem, padding10, onClick (ModelessCtrl <| Modeless.Touch (Modeless.RawColumnItemId (Id.from "dummy") 0)) ]
            [ t "Click me to show Modeless 0 (RawColumnItem)" ]
        , button [ flexItem, padding10, onClick (ModelessCtrl <| Modeless.Touch (Modeless.RawColumnItemId (Id.from "dummy") 1)) ]
            [ t "Click me to show Modeless 1 (RawColumnItem)" ]
        , let
            freshMediaViewerId =
                Modeless.MediaViewerId (Modeless.MediaViewrIdPayload (Id.from "dummy") 2 0 (SelectArray.size m.mediaArray) False)
          in
          button [ flexItem, padding10, onClick (ModelessCtrl (Modeless.Touch freshMediaViewerId)) ]
            [ t "Click me to show Modeless 2 (MediaViewer)" ]
        ]
    , Modeless.render
        { onCloseButtonClick = ModelessCtrl << Modeless.Remove
        , onAnywhereClick = ModelessCtrl << Modeless.Touch
        , onDrag = \\mId x y -> ModelessCtrl (Modeless.Move mId x y)
        , onDragEnd = ModelessCtrl << Modeless.Touch
        , mediaViewerEffects =
            \\mId ->
                { onPagerClick = ModelessCtrl << Modeless.MediaViewerSelectAt mId
                , onToggleSizeClick = ModelessCtrl << Modeless.MediaViewerToggleSize mId
                }
        }
        (Modeless.map (dummyModelessResolver m) m.modeless)
    ]""" <|
            div []
                [ div [ flexColumn, spacingColumn10 ]
                    [ button [ flexItem, padding10, onClick (ModelessCtrl <| Modeless.Touch (Modeless.RawColumnItemId (Id.from "dummy") 0)) ]
                        [ t "Click me to show Modeless 0 (RawColumnItem)" ]
                    , button [ flexItem, padding10, onClick (ModelessCtrl <| Modeless.Touch (Modeless.RawColumnItemId (Id.from "dummy") 1)) ]
                        [ t "Click me to show Modeless 1 (RawColumnItem)" ]
                    , let
                        freshMediaViewerId =
                            Modeless.MediaViewerId (Modeless.MediaViewrIdPayload (Id.from "dummy") 2 0 False)
                      in
                      button [ flexItem, padding10, onClick (ModelessCtrl (Modeless.Touch freshMediaViewerId)) ]
                        [ t "Click me to show Modeless 2 (MediaViewer)" ]
                    ]
                , Modeless.render
                    { onCloseButtonClick = ModelessCtrl << Modeless.Remove
                    , onAnywhereClick = ModelessCtrl << Modeless.Touch
                    , onDrag = \mId x y -> ModelessCtrl (Modeless.Move mId x y)
                    , onDragEnd = ModelessCtrl << Modeless.Touch
                    , mediaViewerEffects =
                        \mId ->
                            { onPagerClick = ModelessCtrl << Modeless.MediaViewerSelectAt mId
                            , onToggleSizeClick = ModelessCtrl << Modeless.MediaViewerToggleSize mId
                            }
                    }
                    (Modeless.map (dummyModelessResolver m) m.modeless)
                ]
        ]


dummyModelessResolver : Model -> Modeless.ModelessId -> Modeless.ResolvedPayload
dummyModelessResolver m mId =
    case mId of
        Modeless.RawColumnItemId _ _ ->
            Modeless.RawColumnItem mId <|
                SystemMessage
                    { id = Modeless.idStr mId
                    , message = Modeless.idStr mId ++ lorem ++ iroha
                    , mediaMaybe = Just (Data.Column.Image (StringExtra.toUrlUnsafe "https://example.com/image.png"))
                    }

        Modeless.MediaViewerId payload ->
            -- Not directly using mediaArray's current selected, rather resolve from payload.mediaIndex
            Modeless.MediaViewer mId <|
                { selectedMedia = SelectArray.selected (SelectArray.selectAt payload.mediaIndex m.mediaArray)
                , mediaIndex = payload.mediaIndex
                , nMedia = SelectArray.size m.mediaArray
                , isShrunk = payload.isShrunk
                }


configPref : Model -> Html Msg
configPref m =
    section []
        [ h1 [ xxProminent ] [ t "Config.Pref" ]
        , withSource """let
    dummyShadowColumn index =
        { id = Id.from (String.fromInt index)
        , pinned = False
        , sources =
            case modBy 3 index of
                0 ->
                    []

                1 ->
                    [ DiscordSource { id = "CID" ++ String.fromInt index, name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]

                _ ->
                    [ SlackSource { id = "CID" ++ String.fromInt index, name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True } ]
        , filters =
            case modBy 2 index of
                0 ->
                    []

                _ ->
                    [ "\\"Elm\\"", "Has Media" ]
        }
in
Pref.render
    { onZephyrModeChange = Toggle
    , onShowColumnButtonClick = always NoOp
    , onDeleteColumnButtonClick = always NoOp
    }
    { zephyrMode = m.toggle
    , evictThreshold = 5
    , columnSlotsAvailable = m.toggle
    , shadowColumns = List.range 0 6 |> List.map dummyShadowColumn
    }""" <|
            let
                dummyShadowColumn index =
                    { id = Id.from (String.fromInt index)
                    , pinned = False
                    , sources =
                        case modBy 3 index of
                            0 ->
                                []

                            1 ->
                                [ DiscordSource { id = "CID" ++ String.fromInt index, name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 20 20) } ]

                            _ ->
                                [ SlackSource { id = "CID" ++ String.fromInt index, name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True } ]
                    , filters =
                        case modBy 2 index of
                            0 ->
                                []

                            _ ->
                                [ "\"Elm\"", "Has Media" ]
                    }
            in
            Pref.render
                { onZephyrModeChange = Toggle
                , onShowColumnButtonClick = always NoOp
                , onDeleteColumnButtonClick = always NoOp
                }
                { zephyrMode = m.toggle
                , evictThreshold = 5
                , columnSlotsAvailable = m.toggle
                , shadowColumns = List.range 0 6 |> List.map dummyShadowColumn
                }
        ]


configStatus : Model -> Html Msg
configStatus m =
    section []
        [ h1 [ xxProminent ] [ t "Config.Status" ]
        , withSource """Status.render
    { itemBrokerCapacity = 5000
    , columnItemLimit = 2000
    , numColumns = m.numColumns
    , numVisible = m.numColumns
    , numPinned = 0
    , clientHeight = 900
    , clientWidth = 1600
    , serviceWorkerAvailable = m.toggle
    , indexedDBAvailable = m.toggle
    }""" <|
            Status.render
                { itemBrokerCapacity = 5000
                , columnItemLimit = 2000
                , numColumns = m.numColumns
                , numVisible = m.numColumns
                , numPinned = 0
                , clientHeight = 900
                , clientWidth = 1600
                , serviceWorkerAvailable = m.toggle
                , indexedDBAvailable = m.toggle
                }
        ]


configDiscord : Model -> Html Msg
configDiscord m =
    section []
        [ h1 [ xxProminent ] [ t "Config.Discord" ]
        , withSource """let
    dummyOpts =
        { rehydrating = m.toggle
        , user = DiscordUser.new
        , guilds = List.range 0 10 |> List.map dummyGuild |> List.map (\\g -> ( DiscordGuild.getId g, g )) |> Dict.fromList
        , subbableChannels = List.range 0 20 |> List.map subbableChannel
        , subbedChannels = List.range 0 15 |> List.map dummyChannel
        }

    dummyGuild index =
        DiscordGuild.new
            |> DiscordGuild.setId (Id.from ("DUMMYGUILDID" ++ String.fromInt index))
            |> DiscordGuild.setName (String.fromInt index ++ "GUILD")

    dummyChannel index =
        { id = Id.from ("DUMMYCHANNELID" ++ String.fromInt index)
        , name = String.join " " (List.repeat (modBy 4 index + 1) ("Channel" ++ String.fromInt index))
        , guildMaybe = Just (dummyGuild (modBy 3 index))
        , fetching = modBy 2 index == 0
        , producing = index /= 0
        }

    subbableChannel index =
        dummyChannel index
            |> (\\{ id, name, guildMaybe } -> { id = id, name = name, guildMaybe = guildMaybe })
in
Discord.render
    { onTokenInput = TextInput
    , onTokenSubmit = Toggle False
    , onRehydrateButtonClick = Toggle (not m.toggle)
    , onChannelSelect = always NoOp
    , onForceFetchButtonClick = always NoOp
    , onCreateColumnButtonClick = always NoOp
    , onUnsubscribeButtonClick = always NoOp
    }
    { token = m.textInput
    , tokenSubmitButtonText = "Submit"
    , tokenSubmittable = True
    , currentState = Discord.HydratedOnce dummyOpts
    , selectMsgTagger = SelectCtrl
    , selectState = m.select
    }""" <|
            let
                dummyOpts =
                    { rehydrating = m.toggle
                    , user = DiscordUser.new
                    , guilds = List.range 0 10 |> List.map dummyGuild |> List.map (\g -> ( DiscordGuild.getId g, g )) |> Dict.fromList
                    , subbableChannels = List.range 0 20 |> List.map subbableChannel
                    , subbedChannels = List.range 0 15 |> List.map dummyChannel
                    }

                dummyGuild index =
                    DiscordGuild.new
                        |> DiscordGuild.setId (Id.from ("DUMMYGUILDID" ++ String.fromInt index))
                        |> DiscordGuild.setName (String.fromInt index ++ "GUILD")

                dummyChannel index =
                    { id = Id.from ("DUMMYCHANNELID" ++ String.fromInt index)
                    , name = String.join " " (List.repeat (modBy 4 index + 1) ("Channel" ++ String.fromInt index))
                    , guildMaybe = Just (dummyGuild (modBy 3 index))
                    , fetching = modBy 2 index == 0
                    , producing = index /= 0
                    }

                subbableChannel index =
                    dummyChannel index
                        |> (\{ id, name, guildMaybe } -> { id = id, name = name, guildMaybe = guildMaybe })
            in
            Discord.render
                { onTokenInput = TextInput
                , onTokenSubmit = Toggle False
                , onRehydrateButtonClick = Toggle (not m.toggle)
                , onChannelSelect = always NoOp
                , onForceFetchButtonClick = always NoOp
                , onCreateColumnButtonClick = always NoOp
                , onUnsubscribeButtonClick = always NoOp
                , selectMsgTagger = SelectCtrl
                }
                { token = m.textInput
                , tokenSubmitButtonText = "Submit"
                , tokenSubmittable = True
                , currentState = Discord.HydratedOnce dummyOpts
                , selectState = m.select
                }
        ]


configSlack : Model -> Html Msg
configSlack m =
    section [ aubergine ]
        [ h1 [ xxProminent ] [ t "Config.Slack" ]
        , withSource """let
    dummyTeamState index =
        if index == 0 then
            ( dummyTeam index, Slack.NowHydrating (dummyUser index) )

        else
            ( dummyTeam index
            , Slack.HydratedOnce
                { rehydrating = m.toggle
                , user = dummyUser index
                , subbableConvos = List.range 0 (index * 3) |> List.map subbableConvo
                , subbedConvos = List.range 0 (index * 3) |> List.map dummyConvo
                }
            )

    dummyTeam index =
        { id = Id.from ("DUMMYTEAMID" ++ String.fromInt index)
        , name = String.fromInt index ++ "TEAM"
        , domain = String.fromInt index ++ "team"
        , image44 =
            if modBy 2 index == 0 then
                Just (Image.ph 48 48)

            else
                Nothing
        }

    dummyUser index =
        { realName = "REAL NAME"
        , displayName =
            case modBy 3 index of
                0 ->
                    Just "nickname"

                1 ->
                    Just (String.join " " (List.repeat 5 "longnickname"))

                _ ->
                    Nothing
        , image48 = Image.ph 48 48
        }

    dummyConvo index =
        { id = Id.from ("DUMMYCONVID" ++ String.fromInt index)
        , name = String.join " " (List.repeat (modBy 4 index + 1) ("Channel" ++ String.fromInt index))
        , isPrivate = modBy 4 index == 0
        , fetching = modBy 2 index == 0
        , producing = index /= 0
        }

    subbableConvo index =
        dummyConvo index
            |> (\\{ id, name, isPrivate } -> { id = id, name = name, isPrivate = isPrivate })
in
Slack.render
    { onTokenInput = TextInput
    , onTokenSubmit = Toggle False
    , onRehydrateButtonClick = Toggle (not m.toggle)
    , onConvSelect = always NoOp
    , onForceFetchButtonClick = always NoOp
    , onCreateColumnButtonClick = always NoOp
    , onUnsubscribeButtonClick = always NoOp
    }
    { token = m.textInput
    , tokenSubmittable = True
    , teamStates = List.range 0 2 |> List.map dummyTeamState
    , selectMsgTagger = SelectCtrl
    , selectState = m.select
    }""" <|
            let
                dummyTeamState index =
                    if index == 0 then
                        ( dummyTeam index, Slack.NowHydrating (dummyUser index) )

                    else
                        ( dummyTeam index
                        , Slack.HydratedOnce
                            { rehydrating = m.toggle
                            , user = dummyUser index
                            , subbableConvos = List.range 0 (index * 3) |> List.map subbableConvo
                            , subbedConvos = List.range 0 (index * 3) |> List.map dummyConvo
                            }
                        )

                dummyTeam index =
                    { id = Id.from ("DUMMYTEAMID" ++ String.fromInt index)
                    , name = String.fromInt index ++ "TEAM"
                    , domain = String.fromInt index ++ "team"
                    , image44 =
                        if modBy 2 index == 0 then
                            Just (Image.ph 48 48)

                        else
                            Nothing
                    }

                dummyUser index =
                    { realName = "REAL NAME"
                    , displayName =
                        case modBy 3 index of
                            0 ->
                                Just "nickname"

                            1 ->
                                Just (String.join " " (List.repeat 5 "longnickname"))

                            _ ->
                                Nothing
                    , image48 = Image.ph 48 48
                    }

                dummyConvo index =
                    { id = Id.from ("DUMMYCONVID" ++ String.fromInt index)
                    , name = String.join " " (List.repeat (modBy 4 index + 1) ("Channel" ++ String.fromInt index))
                    , isPrivate = modBy 4 index == 0
                    , fetching = modBy 2 index == 0
                    , producing = index /= 0
                    }

                subbableConvo index =
                    dummyConvo index
                        |> (\{ id, name, isPrivate } -> { id = id, name = name, isPrivate = isPrivate })
            in
            Slack.render
                { onTokenInput = TextInput
                , onTokenSubmit = Toggle False
                , onRehydrateButtonClick = always (Toggle (not m.toggle))
                , onConvoSelect = \_ _ -> NoOp
                , onForceFetchButtonClick = \_ _ -> NoOp
                , onCreateColumnButtonClick = always NoOp
                , onUnsubscribeButtonClick = \_ _ -> NoOp
                , selectMsgTagger = SelectCtrl
                }
                { token = m.textInput
                , tokenSubmittable = True
                , teamStates = List.range 0 2 |> List.map dummyTeamState
                , selectState = m.select
                }
        ]


columnHeader : Model -> Html Msg
columnHeader m =
    section []
        [ h1 [ xxProminent ] [ t "Column.Header" ]
        , section [ oneDark ]
            [ h2 [ xProminent ] [ t "oneDark" ]
            , withSourceInColumn Nothing 60 """Header.render
    { onColumnDragStart = \\_ _ _ -> NoOp
    , onHeaderClickWhenScrolled = always NoOp
    , onPinButtonClick = \\_ to -> Toggle to
    , onConfigToggleButtonClick = \\_ to -> Toggle to
    , onDismissButtonClick = always NoOp
    }
    0
    { id = Id.from "DUMMYID"
    , sources = [ DiscordSource { id = "ID1", name = "Channel1", guildName = "Guild", guildIcon = Just (Image.ph 40 40) } ]
    , filters = [ "\\"Elm\\"", "Has Media" ]
    , pinned = m.toggle
    , configOpen = m.toggle
    , scrolled = False
    }""" <|
                Header.render
                    { onColumnDragStart = \_ _ _ -> NoOp
                    , onHeaderClickWhenScrolled = always NoOp
                    , onPinButtonClick = \_ to -> Toggle to
                    , onConfigToggleButtonClick = \_ to -> Toggle to
                    , onDismissButtonClick = always NoOp
                    }
                    0
                    { id = Id.from "DUMMYID"
                    , sources = [ DiscordSource { id = "ID1", name = "Channel1", guildName = "Guild", guildIcon = Just (Image.ph 40 40) } ]
                    , filters = [ "\"Elm\"", "Has Media" ]
                    , pinned = m.toggle
                    , configOpen = m.toggle
                    , scrolled = False
                    }
            , withSourceInColumn Nothing 60 """Header.render
    { onColumnDragStart = \\_ _ _ -> NoOp
    , onHeaderClickWhenScrolled = always NoOp
    , onPinButtonClick = \\_ to -> Toggle to
    , onConfigToggleButtonClick = \\_ to -> Toggle to
    , onDismissButtonClick = always NoOp
    }
    0
    { id = Id.from "DUMMYID"
    , sources = []
    , filters = [ "\\"Elm\\"", "Has Media" ]
    , pinned = m.toggle
    , configOpen = m.toggle
    , scrolled = False
    }""" <|
                Header.render
                    { onColumnDragStart = \_ _ _ -> NoOp
                    , onHeaderClickWhenScrolled = always NoOp
                    , onPinButtonClick = \_ to -> Toggle to
                    , onConfigToggleButtonClick = \_ to -> Toggle to
                    , onDismissButtonClick = always NoOp
                    }
                    0
                    { id = Id.from "DUMMYID"
                    , sources = []
                    , filters = [ "\"Elm\"", "Has Media" ]
                    , pinned = m.toggle
                    , configOpen = m.toggle
                    , scrolled = False
                    }
            ]
        , section [ aubergine ]
            [ h2 [ xProminent ] [ t "aubergine" ]
            , withSourceInColumn Nothing 60 """Header.render
    { onColumnDragStart = \\_ _ _ -> NoOp
    , onHeaderClickWhenScrolled = always NoOp
    , onPinButtonClick = \\_ to -> Toggle to
    , onConfigToggleButtonClick = \\_ to -> Toggle to
    , onDismissButtonClick = always NoOp
    }
    0
    { id = Id.from "DUMMYID"
    , sources =
        [ SlackSource { id = "SID1", name = "Conv1", teamName = "Team", teamIcon = Just (Image.ph 41 41), isPrivate = True }
        , DiscordSource { id = "DID1", name = String.repeat 2 "Expands unless constrained ", guildName = "Guild", guildIcon = Just (Image.ph 40 40) }
        ]
    , filters = [ "\\"Elm\\"", "Has Media" ]
    , pinned = m.toggle
    , configOpen = m.toggle
    , scrolled = False
    }""" <|
                Header.render
                    { onColumnDragStart = \_ _ _ -> NoOp
                    , onHeaderClickWhenScrolled = always NoOp
                    , onPinButtonClick = \_ to -> Toggle to
                    , onConfigToggleButtonClick = \_ to -> Toggle to
                    , onDismissButtonClick = always NoOp
                    }
                    0
                    { id = Id.from "DUMMYID"
                    , sources =
                        [ SlackSource { id = "SID1", name = "Conv1", teamName = "Team", teamIcon = Just (Image.ph 41 41), isPrivate = True }
                        , DiscordSource { id = "DID1", name = String.repeat 2 "Expands unless constrained ", guildName = "Guild", guildIcon = Just (Image.ph 40 40) }
                        ]
                    , filters = [ "\"Elm\"", "Has Media" ]
                    , pinned = m.toggle
                    , configOpen = m.toggle
                    , scrolled = False
                    }
            , withSourceInColumn Nothing 60 """Header.render
    { onColumnDragStart = \\_ _ _ -> NoOp
    , onHeaderClickWhenScrolled = always NoOp
    , onPinButtonClick = \\_ to -> Toggle to
    , onConfigToggleButtonClick = \\_ to -> Toggle to
    , onDismissButtonClick = always NoOp
    }
    0
    { id = Id.from "DUMMYID"
    , sources =
        [ SlackSource
            { id = "CID1"
            , name = String.repeat 3 "Shrinks if constrained "
            , teamName = "Team"
            , teamIcon = Just (Image.ph 41 41)
            , isPrivate = True
            }
        ]
    , filters = List.repeat 3 "Shrinks if constrained"
    , pinned = m.toggle
    , configOpen = m.toggle
    , scrolled = False
    }""" <|
                Header.render
                    { onColumnDragStart = \_ _ _ -> NoOp
                    , onHeaderClickWhenScrolled = always NoOp
                    , onPinButtonClick = \_ to -> Toggle to
                    , onConfigToggleButtonClick = \_ to -> Toggle to
                    , onDismissButtonClick = always NoOp
                    }
                    0
                    { id = Id.from "DUMMYID"
                    , sources =
                        [ SlackSource
                            { id = "CID1"
                            , name = String.repeat 3 "Shrinks if constrained "
                            , teamName = "Team"
                            , teamIcon = Just (Image.ph 41 41)
                            , isPrivate = True
                            }
                        ]
                    , filters = List.repeat 3 "Shrinks if constrained"
                    , pinned = m.toggle
                    , configOpen = m.toggle
                    , scrolled = False
                    }
            ]
        ]


withSourceInColumn : Maybe String -> Int -> String -> Html Msg -> Html Msg
withSourceInColumn selectIdMaybe height_ source_ toRender =
    withSource source_ <|
        div []
            [ t "(Contained)"
            , div
                [ style "width" (px 350)
                , style "height" (px height_)
                , style "overflow" "auto"
                , Border.w1
                , Border.solid
                , Border.colorBg
                , case selectIdMaybe of
                    Just id ->
                        on "scroll" (succeed (SelectCtrl (Select.hideUnsafe id)))

                    Nothing ->
                        noAttr
                ]
                [ toRender ]
            ]


columnConfig : Model -> Html Msg
columnConfig m =
    section []
        [ h1 [ xxProminent ] [ t "Column.Config" ]
        , section [ oneDark ]
            [ h2 [ xProminent ] [ t "oneDark" ]
            , withSourceInColumn (Just (ColumnConfig.selectId (Id.from "DUMMYID1"))) 300 """ColumnConfig.render
    { onCloseButtonClick = always (Toggle False)
    , onColumnDeleteButtonClick = always NoOp
    , onSourceSelect = \\_ _ -> NoOp
    , selectMsgTagger = SelectCtrl
    , onRemoveSourceButtonClick = \\_ _ -> NoOp
    }
    { selectState = m.select
    , availableSourecs =
        [ DiscordSource { id = "DID1", name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
        , DiscordSource { id = "DID2", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
        , SlackSource { id = "SID1", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
        , SlackSource { id = "SID2", name = String.repeat 3 "Slack Conversation ", teamName = "Team", teamIcon = Nothing, isPrivate = False }
        ]
    , column =
        { id = Id.from "DUMMYID1"
        , numItems = 1000
        , pinned = m.toggle
        , sources =
            [ DiscordSource { id = "DID0", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
            , SlackSource { id = "SID0", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
            ]
        , filters = []
        }
    }""" <|
                ColumnConfig.render
                    { onCloseButtonClick = always (Toggle False)
                    , onColumnDeleteButtonClick = always NoOp
                    , onSourceSelect = \_ _ -> NoOp
                    , selectMsgTagger = SelectCtrl
                    , onRemoveSourceButtonClick = \_ _ -> NoOp
                    }
                    { selectState = m.select
                    , availableSourecs =
                        [ DiscordSource { id = "DID1", name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
                        , DiscordSource { id = "DID2", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
                        , SlackSource { id = "SID1", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
                        , SlackSource { id = "SID2", name = String.repeat 3 "Slack Conversation ", teamName = "Team", teamIcon = Nothing, isPrivate = False }
                        ]
                    , column =
                        { id = Id.from "DUMMYID1"
                        , numItems = 1000
                        , pinned = m.toggle
                        , sources =
                            [ DiscordSource { id = "DID0", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
                            , SlackSource { id = "SID0", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
                            ]
                        , filters = []
                        }
                    }
            ]
        , section [ aubergine ]
            [ h2 [ xProminent ] [ t "aubergine" ]
            , withSourceInColumn (Just (ColumnConfig.selectId (Id.from "DUMMYID2"))) 400 """ColumnConfig.render
    { onCloseButtonClick = always (Toggle False)
    , onColumnDeleteButtonClick = always NoOp
    , onSourceSelect = \\_ _ -> NoOp
    , selectMsgTagger = SelectCtrl
    , onRemoveSourceButtonClick = \\_ _ -> NoOp
    }
    { selectState = m.select
    , availableSourecs =
        [ DiscordSource { id = "DID1", name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
        , DiscordSource { id = "DID2", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
        , SlackSource { id = "SID1", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
        , SlackSource { id = "SID2", name = String.repeat 3 "Slack Conversation ", teamName = "Team", teamIcon = Nothing, isPrivate = False }
        ]
    , column =
        { id = Id.from "DUMMYID2"
        , numItems = 1000
        , pinned = m.toggle
        , sources =
            [ DiscordSource { id = "DID0", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
            , SlackSource { id = "SID0", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
            ]
        , filters = []
        }
    }""" <|
                ColumnConfig.render
                    { onCloseButtonClick = always (Toggle False)
                    , onColumnDeleteButtonClick = always NoOp
                    , onSourceSelect = \_ _ -> NoOp
                    , selectMsgTagger = SelectCtrl
                    , onRemoveSourceButtonClick = \_ _ -> NoOp
                    }
                    { selectState = m.select
                    , availableSourecs =
                        [ DiscordSource { id = "DID1", name = "Discord Channel", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
                        , DiscordSource { id = "DID2", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
                        , SlackSource { id = "SID1", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
                        , SlackSource { id = "SID2", name = String.repeat 3 "Slack Conversation ", teamName = "Team", teamIcon = Nothing, isPrivate = False }
                        ]
                    , column =
                        { id = Id.from "DUMMYID2"
                        , numItems = 1000
                        , pinned = m.toggle
                        , sources =
                            [ DiscordSource { id = "DID0", name = String.repeat 4 "Discord Channel ", guildName = "Guild", guildIcon = Just (Image.ph 20 20) }
                            , SlackSource { id = "SID0", name = "Slack Conversation", teamName = "Team", teamIcon = Just (Image.ph 21 21), isPrivate = True }
                            ]
                        , filters = []
                        }
                    }
            ]
        ]


columnNewMessageEditor : Model -> Html Msg
columnNewMessageEditor m =
    section []
        [ h1 [ xxProminent ] [ t "Column.NewMessageEditor" ]
        , section [ oneDark ]
            [ h2 [ xProminent ] [ t "oneDark" ]
            , withSourceInColumn (Just (NewMessageEditor.selectId (Id.from "DUMMYID1"))) 400 """NewMessageEditor.render
    { onEditorSelect = \\_ _ -> NoOp
    , selectMsgTagger = SelectCtrl
    , onTextInput = \\_ str -> TextInput str
    , onInteracted = \\_ action -> EditorInteracted action
    , onResetButtonClick = always EditorReset
    , onDiscardFileButtonClick = always EditorFileDiscard
    , onRequestFileAreaClick = always (EditorFileRequest [ "*/*" ])
    , onFileDrop = \\_ -> EditorFileSelected
    , onSubmit = always EditorReset
    }
    { selectState = m.select
    , column =
        { id = Id.from "DUMMYID1"
        , pinned = False
        , sources =
            [ DiscordSource
                { id = "DID1"
                , name = String.repeat 3 "Discord Channel "
                , guildName = "Guild"
                , guildIcon = Just (Image.ph 14 14)
                }
            ]
        , filters = []
        , userActionOnEditor = m.userActionOnEditor
        , editorSeq = m.editorSeq
        , editors =
            SelectArray.fromLists []
                (DiscordMessageEditor { channelId = Id.from "DID1", buffer = m.textInput, file = m.editorFile })
                [ LocalMessageEditor m.textInput ]
        }
    }""" <|
                NewMessageEditor.render
                    { onEditorSelect = \_ _ -> NoOp
                    , selectMsgTagger = SelectCtrl
                    , onTextInput = \_ str -> TextInput str
                    , onInteracted = \_ action -> EditorInteracted action
                    , onResetButtonClick = always EditorReset
                    , onDiscardFileButtonClick = always EditorFileDiscard
                    , onRequestFileAreaClick = always (EditorFileRequest [ "*/*" ])
                    , onFileDrop = \_ -> EditorFileSelected
                    , onSubmit = always EditorReset
                    }
                    { selectState = m.select
                    , column =
                        { id = Id.from "DUMMYID1"
                        , pinned = False
                        , sources =
                            [ DiscordSource
                                { id = "DID1"
                                , name = String.repeat 3 "Discord Channel "
                                , guildName = "Guild"
                                , guildIcon = Just (Image.ph 14 14)
                                }
                            ]
                        , filters = []
                        , userActionOnEditor = m.userActionOnEditor
                        , editorSeq = m.editorSeq
                        , editors =
                            SelectArray.fromLists []
                                (DiscordMessageEditor { channelId = Id.from "DID1", buffer = m.textInput, file = m.editorFile })
                                [ LocalMessageEditor m.textInput ]
                        }
                    }
            ]
        , section [ aubergine ]
            [ h2 [ xProminent ] [ t "aubergine" ]
            , withSourceInColumn (Just (NewMessageEditor.selectId (Id.from "DUMMYID2"))) 400 """NewMessageEditor.render
    { onEditorSelect = \\_ _ -> NoOp
    , selectMsgTagger = SelectCtrl
    , onTextInput = \\_ str -> TextInput str
    , onInteracted = \\_ action -> EditorInteracted action
    , onResetButtonClick = always EditorReset
    , onDiscardFileButtonClick = always EditorFileDiscard
    , onRequestFileAreaClick = always (EditorFileRequest [ "*/*" ])
    , onFileDrop = \\_ -> EditorFileSelected
    , onSubmit = always EditorReset
    }
    { selectState = m.select
    , column =
        { id = Id.from "DUMMYID2"
        , pinned = False
        , sources = []
        , filters = []
        , userActionOnEditor = m.userActionOnEditor
        , editorSeq = m.editorSeq
        , editors = SelectArray.singleton (LocalMessageEditor m.textInput)
        }
    }""" <|
                NewMessageEditor.render
                    { onEditorSelect = \_ _ -> NoOp
                    , selectMsgTagger = SelectCtrl
                    , onTextInput = \_ str -> TextInput str
                    , onInteracted = \_ action -> EditorInteracted action
                    , onResetButtonClick = always EditorReset
                    , onDiscardFileButtonClick = always EditorFileDiscard
                    , onRequestFileAreaClick = always (EditorFileRequest [ "*/*" ])
                    , onFileDrop = \_ -> EditorFileSelected
                    , onSubmit = always EditorReset
                    }
                    { selectState = m.select
                    , column =
                        { id = Id.from "DUMMYID2"
                        , pinned = False
                        , sources = []
                        , filters = []
                        , userActionOnEditor = m.userActionOnEditor
                        , editorSeq = m.editorSeq
                        , editors = SelectArray.singleton (LocalMessageEditor m.textInput)
                        }
                    }
            ]
        ]


columnItems : Html Msg
columnItems =
    let
        themed theme_ themeStr =
            section [ theme_ ]
                [ h2 [ xProminent ] [ t themeStr ]
                , withSourceInColumn Nothing 100 """Items.render
    { onLoadMoreClick = NoOp
    , onItemSourceButtonClick = \\_ _ -> NoOp
    , onItemRefreshButtonClick = \\_ _ -> NoOp
    , onItemMediaClick = \\_ _ _ -> NoOp
    }
    { timezone = Time.utc, itemGroups = [], columnId = Id.from <| themeStr ++ "01", hasMore = False }""" <|
                    Items.render
                        { onLoadMoreClick = NoOp
                        , onItemSourceButtonClick = \_ _ -> NoOp
                        , onItemRefreshButtonClick = \_ _ -> NoOp
                        , onItemMediaClick = \_ _ _ -> NoOp
                        }
                        { timezone = Time.utc, itemGroups = [], columnId = Id.from <| themeStr ++ "01", hasMore = False }
                , withSourceInColumn Nothing 500 """Items.render
    { onLoadMoreClick = NoOp
    , onItemSourceButtonClick = \\_ _ -> NoOp
    , onItemRefreshButtonClick = \\_ _ -> NoOp
    , onItemMediaClick = \\_ _ _ -> NoOp
    }
    { timezone = Time.utc
    , columnId = Id.from <| themeStr ++ "02"
    , hasMore = False
    , itemGroups =
        List.map unit
            [ ItemForView.new "ci0" 0 (NamedEntity.new "Text") (Plain (lorem ++ " " ++ iroha))
                |> ItemForView.timestamp (Time.millisToPosix 1000)
            , ItemForView.new "ci1" 1 (NamedEntity.new "Longstring") (Plain (String.repeat 50 "significantlylongstring"))
                |> ItemForView.timestamp (Time.millisToPosix 100)
            , ItemForView.new "ci2" 2 (NamedEntity.new "Markdown") (Markdown MarkdownBlocks.sampleSource)
                |> ItemForView.timestamp (Time.millisToPosix 10)
            , ItemForView.new "ci3" 3 (NamedEntity.new "KTS") (Plain "KTS follows")
                |> ItemForView.timestamp (Time.millisToPosix 1)
                |> ItemForView.kts
                    [ ( "Key1", Plain ("Plain text. " ++ iroha) )
                    , ( "キー2", Markdown "Marked up **text**" )
                    ]
            ]
    }""" <|
                    Items.render
                        { onLoadMoreClick = NoOp
                        , onItemSourceButtonClick = \_ _ -> NoOp
                        , onItemRefreshButtonClick = \_ _ -> NoOp
                        , onItemMediaClick = \_ _ _ -> NoOp
                        }
                        { timezone = Time.utc
                        , columnId = Id.from <| themeStr ++ "02"
                        , hasMore = False
                        , itemGroups =
                            List.map unit
                                [ ItemForView.new "ci0" 0 (NamedEntity.new "Text") (Plain (lorem ++ " " ++ iroha))
                                    |> ItemForView.timestamp (Time.millisToPosix 1000)
                                , ItemForView.new "ci1" 1 (NamedEntity.new "Longstring") (Plain (String.repeat 50 "significantlylongstring"))
                                    |> ItemForView.timestamp (Time.millisToPosix 100)
                                , ItemForView.new "ci2" 2 (NamedEntity.new "Markdown") (Markdown MarkdownBlocks.sampleSource)
                                    |> ItemForView.timestamp (Time.millisToPosix 10)
                                , ItemForView.new "ci3" 3 (NamedEntity.new "KTS") (Plain "KTS follows")
                                    |> ItemForView.timestamp (Time.millisToPosix 1)
                                    |> ItemForView.kts
                                        [ ( "Key1", Plain ("Plain text. " ++ iroha) )
                                        , ( "キー2", Markdown "Marked up **text**" )
                                        ]
                                ]
                        }
                , withSourceInColumn Nothing 1000 """Items.render
    { onLoadMoreClick = NoOp
    , onItemSourceButtonClick = \\_ _ -> NoOp
    , onItemRefreshButtonClick = \\_ _ -> NoOp
    , onItemMediaClick = \\_ _ _ -> NoOp
    }
    { timezone = Time.utc
    , columnId = Id.from <| themeStr ++ "03"
    , hasMore = True
    , itemGroups =
        List.map unit
            [ ItemForView.new "ci1" 1 (NamedEntity.new "Attachement") (Plain "With image (contained)")
                |> ItemForView.attachedFiles [ sampleImage500x500 ]
            , ItemForView.new "ci2" 2 (NamedEntity.new "Attachement") (Plain "With image (smaller)")
                |> ItemForView.attachedFiles [ sampleImage100x100 ]
            , ItemForView.new "ci3" 3 (NamedEntity.new "Attachement") (Plain "With image (tall)")
                |> ItemForView.attachedFiles [ sampleImage100x600 ]
            , ItemForView.new "ci4" 4 (NamedEntity.new "Attachement") (Plain "With image (landscape)")
                |> ItemForView.attachedFiles [ sampleImage600x100 ]
            , ItemForView.new "ci5" 5 (NamedEntity.new "Attachement") (Plain "With video (contained)")
                |> ItemForView.attachedFiles [ sampleVideo ]
            , ItemForView.new "ci6" 6 (NamedEntity.new "Attachement") (Plain "External file")
                |> ItemForView.attachedFiles [ attachedOther (ExternalLink (Image.ph 100 100)) ]
            , ItemForView.new "ci7" 7 (NamedEntity.new "Attachement") (Plain "Downloadable file (same origin)")
                |> ItemForView.attachedFiles [ attachedOther (DownloadUrl "/index.html") ]
            , ItemForView.new "ci8" 8 (NamedEntity.new "Attachement") (Plain "Downloadable file (cross origin)")
                |> ItemForView.attachedFiles [ attachedOther (DownloadUrl (Image.ph 100 100)) ]
            , ItemForView.new "ci9" 9 (NamedEntity.new "Attachement") (Plain "File with significantly long description")
                |> ItemForView.attachedFiles
                    [ attachedOther (DownloadUrl "/image.html")
                        |> attachedFileDescription (String.repeat 10 "longstring")
                    ]
            , ItemForView.new "ci10" 10 (NamedEntity.new "Attachement") (Plain "External file (with preview)")
                |> ItemForView.attachedFiles
                    [ attachedOther (ExternalLink "/index.html")
                        |> attachedFilePreview samplePreview
                    ]
            , ItemForView.new "ci11" 11 (NamedEntity.new "Attachement") (Plain "Multiple images")
                |> ItemForView.attachedFiles [ sampleImage500x500, sampleImage600x100 ]
            , ItemForView.new "ci12" 12 (NamedEntity.new "Attachement") (Plain "YouTube without poster")
                |> ItemForView.attachedFiles [ attachedYoutube "F9vhni6eNR8" ]
            , ItemForView.new "ci13" 13 (NamedEntity.new "Attachement") (Plain "YouTube with poster")
                |> ItemForView.attachedFiles [ attachedYoutube "F9vhni6eNR8" |> attachedFilePoster (Image.ph 300 200) ]
            , ItemForView.new "ci14" 14 (NamedEntity.new "Attachement") (Plain "Twitch channel without poster")
                |> ItemForView.attachedFiles [ attachedTwitchChannel "followgrubby" ]
            , ItemForView.new "ci15" 15 (NamedEntity.new "Attachement") (Plain "Twitch channel with poster")
                |> ItemForView.attachedFiles [ attachedTwitchChannel "followgrubby" |> attachedFilePoster (Image.ph 300 200) ]
            ]
    }""" <|
                    let
                        sampleImage500x500 =
                            attachedImage (Image.ph 500 500) |> attachedFileDimension { width = 500, height = 500 }

                        sampleImage100x100 =
                            attachedImage (Image.ph 100 100) |> attachedFileDimension { width = 100, height = 100 }

                        sampleImage100x600 =
                            attachedImage (Image.ph 100 600) |> attachedFileDimension { width = 100, height = 600 }

                        sampleImage600x100 =
                            attachedImage (Image.ph 600 100) |> attachedFileDimension { width = 600, height = 100 }

                        sampleVideo =
                            attachedVideo "https://archive.org/download/BigBuckBunny_124/Content/big_buck_bunny_720p_surround.mp4"

                        samplePreview =
                            """<!DOCTYPE HTML>
<html>
    <head>
      <meta charset="UTF-8">
      <title>Sample</title>
    </head>
    <body>
        Hi!
    </body>
</html>
"""
                    in
                    Items.render
                        { onLoadMoreClick = NoOp
                        , onItemSourceButtonClick = \_ _ -> NoOp
                        , onItemRefreshButtonClick = \_ _ -> NoOp
                        , onItemMediaClick = \_ _ _ -> NoOp
                        }
                        { timezone = Time.utc
                        , columnId = Id.from <| themeStr ++ "03"
                        , hasMore = True
                        , itemGroups =
                            List.map unit
                                [ ItemForView.new "ci1" 1 (NamedEntity.new "Attachement") (Plain "With image (contained)")
                                    |> ItemForView.attachedFiles [ sampleImage500x500 ]
                                , ItemForView.new "ci2" 2 (NamedEntity.new "Attachement") (Plain "With image (smaller)")
                                    |> ItemForView.attachedFiles [ sampleImage100x100 ]
                                , ItemForView.new "ci3" 3 (NamedEntity.new "Attachement") (Plain "With image (tall)")
                                    |> ItemForView.attachedFiles [ sampleImage100x600 ]
                                , ItemForView.new "ci4" 4 (NamedEntity.new "Attachement") (Plain "With image (landscape)")
                                    |> ItemForView.attachedFiles [ sampleImage600x100 ]
                                , ItemForView.new "ci5" 5 (NamedEntity.new "Attachement") (Plain "With video (contained)")
                                    |> ItemForView.attachedFiles [ sampleVideo ]
                                , ItemForView.new "ci6" 6 (NamedEntity.new "Attachement") (Plain "External file")
                                    |> ItemForView.attachedFiles [ attachedOther (ExternalLink (Image.ph 100 100)) ]
                                , ItemForView.new "ci7" 7 (NamedEntity.new "Attachement") (Plain "Downloadable file (same origin)")
                                    |> ItemForView.attachedFiles [ attachedOther (DownloadUrl "/index.html") ]
                                , ItemForView.new "ci8" 8 (NamedEntity.new "Attachement") (Plain "Downloadable file (cross origin)")
                                    |> ItemForView.attachedFiles [ attachedOther (DownloadUrl (Image.ph 100 100)) ]
                                , ItemForView.new "ci9" 9 (NamedEntity.new "Attachement") (Plain "File with significantly long description")
                                    |> ItemForView.attachedFiles
                                        [ attachedOther (DownloadUrl "/image.html")
                                            |> attachedFileDescription (String.repeat 10 "longstring")
                                        ]
                                , ItemForView.new "ci10" 10 (NamedEntity.new "Attachement") (Plain "External file (with preview)")
                                    |> ItemForView.attachedFiles
                                        [ attachedOther (ExternalLink "/index.html")
                                            |> attachedFilePreview samplePreview
                                        ]
                                , ItemForView.new "ci11" 11 (NamedEntity.new "Attachement") (Plain "Multiple images")
                                    |> ItemForView.attachedFiles [ sampleImage500x500, sampleImage600x100 ]
                                , ItemForView.new "ci12" 12 (NamedEntity.new "Attachement") (Plain "YouTube without poster")
                                    |> ItemForView.attachedFiles [ attachedYoutube "F9vhni6eNR8" ]
                                , ItemForView.new "ci13" 13 (NamedEntity.new "Attachement") (Plain "YouTube with poster")
                                    |> ItemForView.attachedFiles [ attachedYoutube "F9vhni6eNR8" |> attachedFilePoster (Image.ph 300 200) ]
                                , ItemForView.new "ci14" 14 (NamedEntity.new "Attachement") (Plain "Twitch channel without poster")
                                    |> ItemForView.attachedFiles [ attachedTwitchChannel "followgrubby" ]
                                , ItemForView.new "ci15" 15 (NamedEntity.new "Attachement") (Plain "Twitch channel with poster")
                                    |> ItemForView.attachedFiles [ attachedTwitchChannel "followgrubby" |> attachedFilePoster (Image.ph 300 200) ]
                                ]
                        }
                , withSourceInColumn Nothing 300 """Items.render
    { onLoadMoreClick = NoOp
    , onItemSourceButtonClick = \\_ _ -> NoOp
    , onItemRefreshButtonClick = \\_ _ -> NoOp
    , onItemMediaClick = \\_ _ _ -> NoOp
    }
    { timezone = Time.utc
    , columnId = Id.from <| themeStr ++ "04"
    , hasMore = False
    , itemGroups =
        List.map unit
            [ ItemForView.new "ci0" 0 (NamedEntity.new "With Avatar" |> NamedEntity.avatar NamedEntity.OcticonInfo) (Plain "OcticonInfo")
            , ItemForView.new "ci1" 1 (NamedEntity.new "With Avatar" |> NamedEntity.avatar NamedEntity.OcticonNote) (Plain "OcticonNote")
            , ItemForView.new "ci2"
                2
                (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 60 60)) "With Avatar" False))
                (Plain "ImageOrAbbr")
            , ItemForView.new "ci3"
                3
                (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 60 60)) "With Avatar" True))
                (Plain "ImageOrAbbr")
            , ItemForView.new "ci4"
                4
                (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr Nothing "With Avatar" False))
                (Plain "ImageOrAbbr")
            , ItemForView.new "ci5"
                5
                (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr Nothing "With Avatar" True))
                (Plain "ImageOrAbbr")
            ]
    }""" <|
                    Items.render
                        { onLoadMoreClick = NoOp
                        , onItemSourceButtonClick = \_ _ -> NoOp
                        , onItemRefreshButtonClick = \_ _ -> NoOp
                        , onItemMediaClick = \_ _ _ -> NoOp
                        }
                        { timezone = Time.utc
                        , columnId = Id.from <| themeStr ++ "04"
                        , hasMore = False
                        , itemGroups =
                            List.map unit
                                [ ItemForView.new "ci0" 0 (NamedEntity.new "With Avatar" |> NamedEntity.avatar NamedEntity.OcticonInfo) (Plain "OcticonInfo")
                                , ItemForView.new "ci1" 1 (NamedEntity.new "With Avatar" |> NamedEntity.avatar NamedEntity.OcticonNote) (Plain "OcticonNote")
                                , ItemForView.new "ci2"
                                    2
                                    (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 60 60)) "With Avatar" False))
                                    (Plain "ImageOrAbbr")
                                , ItemForView.new "ci3"
                                    3
                                    (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 60 60)) "With Avatar" True))
                                    (Plain "ImageOrAbbr")
                                , ItemForView.new "ci4"
                                    4
                                    (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr Nothing "With Avatar" False))
                                    (Plain "ImageOrAbbr")
                                , ItemForView.new "ci5"
                                    5
                                    (NamedEntity.new "With Avatar" |> NamedEntity.avatar (NamedEntity.imageOrAbbr Nothing "With Avatar" True))
                                    (Plain "ImageOrAbbr")
                                ]
                        }
                , withSourceInColumn Nothing 1000 """Items.render
    { onLoadMoreClick = NoOp
    , onItemSourceButtonClick = \\_ _ -> NoOp
    , onItemRefreshButtonClick = \\_ _ -> NoOp
    , onItemMediaClick = \\_ _ _ -> NoOp
    }
    { timezone = Time.utc
    , columnId = Id.from <| themeStr ++ "05"
    , hasMore = False
    , itemGroups =
        List.map unit
            [ ItemForView.new "ci0" 0 (NamedEntity.new "Embed") (Plain "With embeds")
                |> ItemForView.embeddedMatters
                    [ EmbeddedMatter.new (Plain ("This is body text. " ++ lorem))
                        |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#335577")
                        |> EmbeddedMatter.pretext (Plain "Leading text")
                        |> EmbeddedMatter.author
                            (NamedEntity.new "With Avatar"
                                |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 30 30)) "With Avatar" False)
                                |> NamedEntity.url "https://example.com/user"
                                |> NamedEntity.secondaryName "@with_avatar"
                            )
                        |> EmbeddedMatter.title (Plain "This is title")
                        |> EmbeddedMatter.url "https://example.com/verylongpath/morethan30/index.html"
                        |> EmbeddedMatter.origin
                            (NamedEntity.new "Origin Service"
                                |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 20 20)) "Origin Service" False)
                                |> NamedEntity.url "https://example.com/origin"
                            )
                        |> EmbeddedMatter.kts
                            [ ( "Key1", Plain ("Plain text. " ++ iroha) )
                            , ( "キー2", Markdown "Marked up **text**" )
                            ]
                    ]
            , ItemForView.new "ci1" 1 (NamedEntity.new "Embed") (Plain "With markdowns")
                |> ItemForView.embeddedMatters
                    [ EmbeddedMatter.new (Markdown MarkdownBlocks.sampleSource)
                        |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#773355")
                        |> EmbeddedMatter.author (NamedEntity.new "Without Avatar")
                        |> EmbeddedMatter.title (Markdown "**Marked-up title**")
                        |> EmbeddedMatter.origin (NamedEntity.new "Origin Service" |> NamedEntity.url "https://example.com/origin")
                    ]
            , ItemForView.new "ci2" 2 (NamedEntity.new "Embed") (Plain "With attachement")
                |> ItemForView.embeddedMatters
                    [ EmbeddedMatter.new (Plain ("200x200 image and video, and thumbnail. " ++ lorem))
                        |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#557733")
                        |> EmbeddedMatter.thumbnail (imageMedia (Image.ph 100 100) (Image.ph 100 100) "Thumbnail" (Just { width = 100, height = 100 }))
                        |> EmbeddedMatter.attachedFiles
                            [ attachedImage (Image.ph 200 200) |> attachedFileDimension { width = 200, height = 200 }
                            , attachedVideo "https://archive.org/download/BigBuckBunny_124/Content/big_buck_bunny_720p_surround.mp4"
                            ]
                    ]
            , ItemForView.new "ci3" 3 (NamedEntity.new "Embed") (Plain "Multiple embeds")
                |> ItemForView.embeddedMatters
                    [ EmbeddedMatter.new (Plain lorem) |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#335577") |> EmbeddedMatter.pretext (Plain "First")
                    , EmbeddedMatter.new (Plain iroha) |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#775533") |> EmbeddedMatter.pretext (Plain "Second")
                    ]
            ]
    }""" <|
                    Items.render
                        { onLoadMoreClick = NoOp
                        , onItemSourceButtonClick = \_ _ -> NoOp
                        , onItemRefreshButtonClick = \_ _ -> NoOp
                        , onItemMediaClick = \_ _ _ -> NoOp
                        }
                        { timezone = Time.utc
                        , columnId = Id.from <| themeStr ++ "05"
                        , hasMore = False
                        , itemGroups =
                            List.map unit
                                [ ItemForView.new "ci0" 0 (NamedEntity.new "Embed") (Plain "With embeds")
                                    |> ItemForView.embeddedMatters
                                        [ EmbeddedMatter.new (Plain ("This is body text. " ++ lorem))
                                            |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#335577")
                                            |> EmbeddedMatter.pretext (Plain "Leading text")
                                            |> EmbeddedMatter.author
                                                (NamedEntity.new "With Avatar"
                                                    |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 30 30)) "With Avatar" False)
                                                    |> NamedEntity.url "https://example.com/user"
                                                    |> NamedEntity.secondaryName "@with_avatar"
                                                )
                                            |> EmbeddedMatter.title (Plain "This is title")
                                            |> EmbeddedMatter.url "https://example.com/verylongpath/morethan30/index.html"
                                            |> EmbeddedMatter.origin
                                                (NamedEntity.new "Origin Service"
                                                    |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Image.ph 20 20)) "Origin Service" False)
                                                    |> NamedEntity.url "https://example.com/origin"
                                                )
                                            |> EmbeddedMatter.kts
                                                [ ( "Key1", Plain ("Plain text. " ++ iroha) )
                                                , ( "キー2", Markdown "Marked up **text**" )
                                                ]
                                        ]
                                , ItemForView.new "ci1" 1 (NamedEntity.new "Embed") (Plain "With markdowns")
                                    |> ItemForView.embeddedMatters
                                        [ EmbeddedMatter.new (Markdown MarkdownBlocks.sampleSource)
                                            |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#773355")
                                            |> EmbeddedMatter.author (NamedEntity.new "Without Avatar")
                                            |> EmbeddedMatter.title (Markdown "**Marked-up title**")
                                            |> EmbeddedMatter.origin (NamedEntity.new "Origin Service" |> NamedEntity.url "https://example.com/origin")
                                        ]
                                , ItemForView.new "ci2" 2 (NamedEntity.new "Embed") (Plain "With attachement")
                                    |> ItemForView.embeddedMatters
                                        [ EmbeddedMatter.new (Plain ("200x200 image and video, and thumbnail. " ++ lorem))
                                            |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#557733")
                                            |> EmbeddedMatter.thumbnail (EmbeddedMatter.Thumbnail (Image.ph 100 100) (Image.ph 100 100) "Thumbnail" (Just { width = 100, height = 100 }))
                                            |> EmbeddedMatter.attachedFiles
                                                [ attachedImage (Image.ph 200 200) |> attachedFileDimension { width = 200, height = 200 }
                                                , attachedVideo "https://archive.org/download/BigBuckBunny_124/Content/big_buck_bunny_720p_surround.mp4"
                                                ]
                                        ]
                                , ItemForView.new "ci3" 3 (NamedEntity.new "Embed") (Plain "Multiple embeds")
                                    |> ItemForView.embeddedMatters
                                        [ EmbeddedMatter.new (Plain lorem) |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#335577") |> EmbeddedMatter.pretext (Plain "First")
                                        , EmbeddedMatter.new (Plain iroha) |> EmbeddedMatter.color (ColorExtra.fromHexUnsafe "#775533") |> EmbeddedMatter.pretext (Plain "Second")
                                        ]
                                ]
                        }
                ]

        unit item =
            ( item, [] )
    in
    section []
        [ h1 [ xxProminent ] [ t "Column.Items" ]
        , themed oneDark "oneDark"
        , themed aubergine "aubergine"
        ]


mainTemplate : Model -> List (Html Msg)
mainTemplate m =
    let
        mainEffects =
            { sidebarEffects =
                { onConfigToggleClick = Toggle
                , onAddColumnClick = AddColumn
                , onColumnButtonClickByIndex = always NoOp
                }
            , modelessEffects =
                { onCloseButtonClick = ModelessCtrl << Modeless.Remove
                , onAnywhereClick = ModelessCtrl << Modeless.Touch
                , onDrag = \mId x y -> ModelessCtrl (Modeless.Move mId x y)
                , onDragEnd = ModelessCtrl << Modeless.Touch
                , mediaViewerEffects =
                    \mId ->
                        { onPagerClick = ModelessCtrl << Modeless.MediaViewerSelectAt mId
                        , onToggleSizeClick = ModelessCtrl << Modeless.MediaViewerToggleSize mId
                        }
                }
            , onColumnDragEnd = NoOp
            , onColumnDragHover = always NoOp
            , onColumnBorderFlashEnd = always NoOp
            , columnItemsScrollAttrs = always [ on "scroll" (succeed NoOp) ]
            }

        mainProps =
            { configOpen = m.toggle
            , visibleColumns = dummyColumns
            , modeless = Modeless.map (dummyModelessResolver m) m.modeless
            }

        dummyColumns =
            List.map dummyColumn (List.range 0 (m.numColumns - 1))

        dummyColumn index =
            { id = Id.from (String.fromInt index)
            , pinned = modBy 2 index == 0
            , configOpen =
                if modBy 2 index == 0 then
                    m.toggle

                else
                    not m.toggle
            , dragStatus =
                case modBy 4 index of
                    0 ->
                        Settled

                    1 ->
                        Undroppable

                    2 ->
                        Droppable

                    _ ->
                        Grabbed
            , recentlyTouched =
                case modBy 5 index of
                    0 ->
                        True

                    _ ->
                        False
            , sources =
                case modBy 3 index of
                    0 ->
                        []

                    1 ->
                        [ DiscordSource { id = "CID" ++ String.fromInt index, name = "Channel1", guildName = "Guild", guildIcon = Nothing } ]

                    _ ->
                        [ SlackSource { id = "CID" ++ String.fromInt index, name = "Conv1", teamName = "Team", teamIcon = Nothing, isPrivate = False }
                        , DiscordSource { id = "CID" ++ String.fromInt index, name = "Channel1", guildName = "Guild", guildIcon = Nothing }
                        ]
            , filters =
                case modBy 2 index of
                    0 ->
                        []

                    _ ->
                        [ "\"Elm\"", "Has Media" ]
            }

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
    in
    View.Templates.Main.render mainEffects mainProps <|
        { configContents =
            { pref = div [ flexBasis "400px" ] [ t "PREFERENCE[PH]" ]
            , slack = div [ flexBasis "400px" ] [ t "SLACK[PH]" ]
            , discord = div [ flexBasis "400px" ] [ t "DISCORD[PH]" ]
            , status = div [ flexBasis "400px" ] [ t "STATUS[PH]" ]
            }
        , columnContents =
            { header = \index _ -> div [ xProminent, flexBasis "40px" ] [ t "HEADER[PH] ", t (String.fromInt index) ]
            , config = \_ _ -> div [ Border.w1, Border.solid, flexBasis "200px" ] [ t "CONFIG[PH]" ]
            , newMessageEditor = \_ -> div [ flexBasis "50px" ] [ t "MESSAGE EDITOR[PH]" ]
            , items = \_ -> div [ flexColumn ] <| List.map dummyItem <| List.range 0 10
            }
        }
