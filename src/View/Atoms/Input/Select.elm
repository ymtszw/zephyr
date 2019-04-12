module View.Atoms.Input.Select exposing
    ( State, Msg, init, hideUnsafe, update, sub
    , Options, render, styles
    )

{-| Select Input Atom.

Since this select input is implemented in pure Elm,
it is a component with State and Msg, in order to control its toggle state,
and filtering feature.

In order to "popout" option dropdowns and control their visibilities,
it uses Popout module under the hood. Thus its slightly odd API described below.

@docs State, Msg, init, hideUnsafe, update, sub
@docs Options, render, styles

-}

import Color exposing (cssRgba)
import Debounce exposing (Debounce)
import Extra exposing (emit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.Keyed
import Json.Decode exposing (field, string, succeed)
import Json.DecodeExtra exposing (when)
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Popout as Popout
import View.Atoms.Theme exposing (Theme, aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Style exposing (..)



-- STATE


{-| Global state of select input elements.

Only one select input element can be open at a time within an applicaiton.
Therefore you should only have one instance of this type in your application's model.

-}
type State
    = State
        { opened : Maybe Opened
        , popoutState : Popout.State
        }


type alias Opened =
    { filter : String
    , filterSettled : String
    , filterDebouncer : Debounce String
    }


init : State
init =
    State
        { opened = Nothing
        , popoutState = Popout.init
        }


initOpened : Opened
initOpened =
    { filter = ""
    , filterSettled = ""
    , filterDebouncer = Debounce.init
    }



-- UPDATE


type Msg msg
    = Pick Popout.Msg msg
    | FilterInput String
    | FilterSettle String
    | DebounceMsg Debounce.Msg
    | PopoutMsg Popout.Msg


hideUnsafe : String -> Msg msg
hideUnsafe idStr =
    PopoutMsg (Popout.hideUnsafe idStr)


update : (Msg msg -> msg) -> Msg msg -> State -> ( State, Cmd msg )
update msgTagger msg (State s) =
    case msg of
        Pick pMsg toEmit ->
            let
                ( newState, cmd ) =
                    handlePopoutMsg msgTagger pMsg (State s)
            in
            ( newState, Cmd.batch [ cmd, emit toEmit ] )

        FilterInput filter ->
            case s.opened of
                Just opened ->
                    let
                        ( deb, cmd ) =
                            Debounce.push filterDebouncerConfig filter opened.filterDebouncer
                    in
                    ( State { s | opened = Just { opened | filter = filter, filterDebouncer = deb } }
                    , Cmd.map msgTagger cmd
                    )

                Nothing ->
                    ( State s, Cmd.none )

        FilterSettle filterSettled ->
            case s.opened of
                Just opened ->
                    ( State { s | opened = Just { opened | filterSettled = filterSettled } }, Cmd.none )

                Nothing ->
                    ( State s, Cmd.none )

        DebounceMsg dMsg ->
            case s.opened of
                Just opened ->
                    let
                        ( deb, cmd ) =
                            Debounce.update filterDebouncerConfig emitOnSettle dMsg opened.filterDebouncer
                    in
                    ( State { s | opened = Just { opened | filterDebouncer = deb } }
                    , Cmd.map msgTagger cmd
                    )

                Nothing ->
                    ( State s, Cmd.none )

        PopoutMsg pMsg ->
            handlePopoutMsg msgTagger pMsg (State s)


filterDebouncerConfig : Debounce.Config (Msg msg)
filterDebouncerConfig =
    Debounce.Config (Debounce.later settleMillis) DebounceMsg


settleMillis : Float
settleMillis =
    500


emitOnSettle : Debounce.Send String (Msg msg)
emitOnSettle =
    Debounce.takeLast (emit << FilterSettle)


handlePopoutMsg : (Msg msg -> msg) -> Popout.Msg -> State -> ( State, Cmd msg )
handlePopoutMsg msgTagger pMsg (State s) =
    let
        ( popoutState, pCmd ) =
            Popout.update pMsg s.popoutState

        newOpened =
            case ( s.opened, Popout.allClosed popoutState ) of
                ( Just _, True ) ->
                    Nothing

                ( Just _, False ) ->
                    s.opened

                ( Nothing, True ) ->
                    Nothing

                ( Nothing, False ) ->
                    Just initOpened
    in
    ( State { s | opened = newOpened, popoutState = popoutState }
    , Cmd.map (msgTagger << PopoutMsg) pCmd
    )


sub : (Msg msg -> msg) -> State -> Sub msg
sub msgTagger (State s) =
    Sub.map (msgTagger << PopoutMsg) (Popout.sub s.popoutState)



-- VIEW


type alias Options a msg =
    { state : State
    , msgTagger : Msg msg -> msg
    , id : String
    , thin : Bool
    , onSelect : a -> msg
    , selectedOption : Maybe a
    , filterMatch : Maybe (String -> a -> Bool)
    , options : List ( String, a )
    , optionHtml : a -> Html msg
    }


{-| Select input element.

Require `id` and `state` to control open/closed status.
Also, it uess Html.Keyed.

By default it is a block element.

-}
render : List (Attribute msg) -> Options a msg -> Html msg
render userAttrs opts =
    let
        attrs =
            [ class selectClass
            , attribute "role" <|
                case opts.filterMatch of
                    Just _ ->
                        "combobox"

                    Nothing ->
                        "listbox"
            ]
                ++ userAttrs
    in
    Popout.render (optionsDropdown opts) <|
        \control ->
            Popout.node "div" attrs [ header control.toggle opts ]


header : msg -> Options a msg -> Html msg
header onPress opts =
    div
        [ id opts.id
        , flexRow
        , flexCenter
        , headerPadding opts.thin
        , spacingRow2
        , Border.round5
        , Background.colorNote
        , Cursor.pointer
        , onClick onPress
        , onEnterKeyDown onPress
        , tabindex 0
        ]
        [ div [ class headerTextClass, flexGrow ]
            [ Maybe.withDefault (text "Select...") (Maybe.map opts.optionHtml opts.selectedOption) ]
        , div [ class headerChevronClass, Border.rightRound5, Background.colorSub ]
            [ Image.octicon { size = headerChevronSize, shape = Octicons.chevronDown } ]
        ]


onEnterKeyDown : msg -> Attribute msg
onEnterKeyDown onPress =
    on "keydown" (when (field "key" string) ((==) "Enter") (succeed onPress))


headerPadding : Bool -> Attribute msg
headerPadding thin =
    if thin then
        noAttr

    else
        padding5


optionsDropdown : Options a msg -> Popout.Popout msg
optionsDropdown opts =
    let
        config =
            { id = opts.id
            , msgTagger = opts.msgTagger << PopoutMsg
            , orientation = Popout.anchoredVerticallyTo opts.id
            }

        (State s) =
            opts.state
    in
    Popout.generate config s.popoutState <|
        \_ ->
            let
                contents =
                    case ( s.opened, opts.filterMatch ) of
                        ( Just opened, Just _ ) ->
                            [ optionFilter (opts.msgTagger << FilterInput) opened.filter
                            , optionList opts opened
                            ]

                        ( Just opened, Nothing ) ->
                            [ optionList opts opened ]

                        ( Nothing, _ ) ->
                            []
            in
            Popout.node "div"
                [ class optionsClass
                , Border.round5
                , Background.colorNote
                ]
                contents


optionFilter : (String -> msg) -> String -> Html msg
optionFilter onInput_ filter =
    Html.input
        [ type_ "text"
        , class optionFilterClass
        , placeholder "Filter"
        , onInput onInput_
        , widthFill
        , padding2
        , Border.y1
        , Border.solid
        , Background.colorNote
        , value filter
        ]
        []


optionList : Options a msg -> Opened -> Html msg
optionList opts opened =
    Html.Keyed.node "div" [ class optionListClass, flexColumn ] <|
        List.map (optionRowKey opts) <|
            case ( opts.filterMatch, not (String.isEmpty opened.filterSettled) ) of
                ( Just matcher, True ) ->
                    List.filter (Tuple.second >> matcher opened.filterSettled) opts.options

                _ ->
                    opts.options


optionRowKey : Options a msg -> ( String, a ) -> ( String, Html msg )
optionRowKey opts ( optionKey, option ) =
    let
        onSelect =
            opts.msgTagger (Pick (Popout.hideUnsafe opts.id) (opts.onSelect option))
    in
    Tuple.pair optionKey <|
        div
            [ class optionRowClass
            , attribute "role" "option"
            , Cursor.pointer
            , tabindex 0
            , onClick onSelect
            , onEnterKeyDown onSelect
            , if opts.selectedOption == Just option then
                Background.colorPrim

              else
                noAttr
            ]
            [ opts.optionHtml option ]



-- STYLES


styles : List Style
styles =
    [ s_ (c selectClass) [ ( "max-width", "50vw" ) ] -- Global maximum width for select
    , s_ (c headerTextClass)
        [ ( "white-space", "nowrap" )
        , ( "overflow", "hidden" )
        , ( "text-overflow", "ellipsis" )
        , ( "padding-left", px headerTextPaddingX )
        , ( "padding-right", px headerTextPaddingX )
        , ( "min-width", "0" )
        , ( "user-select", "none" )
        ]
    , s_ (c headerChevronClass)
        [ ( "width", px headerChevronSize )
        , ( "height", px headerChevronSize )
        ]
    , s_ (c optionsClass)
        [ ( "position", "absolute" )
        , ( "z-index", "20" ) -- Pop above all else
        , ( "box-shadow", "5px 5px 10px 0px " ++ cssRgba oneDarkTheme.bg )
        , ( "padding-top", px optionListPaddingY )
        , ( "padding-bottom", px optionListPaddingY )
        ]
    , s_ (c optionListClass)
        [ ( "overflow-y", "auto" )
        , ( "overflow-x", "hidden" )
        , ( "max-height", px optionListMaxHeight )
        , ( "min-width", px optionListMinWidth )
        , ( "max-width", "50vw" )
        ]
    , s_ (c optionRowClass)
        [ ( "padding", px optionRowPaddingY ++ " " ++ px headerTextPaddingX )
        ]
    ]
        ++ themedStyles oneDarkClass oneDarkTheme
        ++ themedStyles aubergineClass aubergineTheme


s_ : String -> List ( String, String ) -> Style
s_ =
    View.Style.s


themedStyles : String -> Theme -> List Style
themedStyles themeClass theme =
    [ scoped (c themeClass) (c selectClass) [ ( "color", cssRgba theme.text ) ]
    , let
        focusSelector =
            String.join "," <|
                List.map (\pseudo -> descOf (c themeClass) (c optionRowClass) ++ pseudo) [ ":hover", ":focus" ]
      in
      s_ focusSelector [ ( "background-color", cssRgba theme.sub ) ]
    ]


selectClass : String
selectClass =
    "sl"


headerTextClass : String
headerTextClass =
    "slhtxt"


headerTextPaddingX : Int
headerTextPaddingX =
    3


headerChevronClass : String
headerChevronClass =
    "slchev"


headerChevronSize : Int
headerChevronSize =
    20


optionsClass : String
optionsClass =
    "slopts"


optionListMinWidth : Int
optionListMinWidth =
    100


optionListMaxHeight : Int
optionListMaxHeight =
    300


optionListPaddingY : Int
optionListPaddingY =
    5


optionFilterClass : String
optionFilterClass =
    "slfltr"


optionListClass : String
optionListClass =
    "slopli"


optionRowClass : String
optionRowClass =
    "slopro"


optionRowPaddingY : Int
optionRowPaddingY =
    2
