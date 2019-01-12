module View.Atom.Input.Select exposing
    ( State(..), Msg(..), update, sub
    , Options, select, styles
    )

{-| Select Input Atom.

Since this select input is implemented in pure Elm,
it is a component with State and Msg, in order to control its toggle state,
and filtering feature.

@docs State, Msg, update, sub
@docs Options, select, styles

-}

import Browser.Events
import Color exposing (cssRgba)
import Debounce exposing (Debounce)
import Extra exposing (emit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, stopPropagationOn)
import Html.Keyed
import Json.Decode exposing (at, fail, field, string, succeed)
import Json.DecodeExtra exposing (when)
import Octicons
import Task
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image as Image
import View.Atom.Layout as Layout
import View.Atom.Theme exposing (Theme, aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Style exposing (..)



-- STATE


{-| Global state of select input elements.

Only one select input element can be open at a time within an applicaiton.
Therefore you should only have one instance of this type in your application's model.

-}
type State
    = Open
        { id : String
        , filter : String
        , filterSettled : String
        , filterDebouncer : Debounce String
        , readyToClose : Bool
        }
    | AllClosed



-- UPDATE


type Msg msg
    = Toggle String Bool
    | ReadyToClose
    | Pick msg
    | FilterInput String
    | FilterSettle String
    | DebounceMsg Debounce.Msg


update : (Msg msg -> msg) -> Msg msg -> State -> ( State, Cmd msg )
update msgTagger msg state =
    case msg of
        Toggle id True ->
            ( Open { id = id, filter = "", filterSettled = "", filterDebouncer = Debounce.init, readyToClose = False }, Cmd.none )

        Toggle _ False ->
            ( AllClosed, Cmd.none )

        ReadyToClose ->
            case state of
                Open record ->
                    ( Open { record | readyToClose = True }, Cmd.none )

                AllClosed ->
                    ( AllClosed, Cmd.none )

        Pick x ->
            ( AllClosed, emit x )

        FilterInput filter ->
            case state of
                Open record ->
                    let
                        ( filterDebouncer, cmd ) =
                            Debounce.push filterDebouncerConfig filter record.filterDebouncer
                    in
                    ( Open { record | filter = filter, filterDebouncer = filterDebouncer }, Cmd.map msgTagger cmd )

                AllClosed ->
                    ( AllClosed, Cmd.none )

        FilterSettle filterSettled ->
            case state of
                Open record ->
                    ( Open { record | filterSettled = filterSettled }, Cmd.none )

                AllClosed ->
                    ( AllClosed, Cmd.none )

        DebounceMsg dMsg ->
            case state of
                Open record ->
                    let
                        ( filterDebouncer, cmd ) =
                            Debounce.update filterDebouncerConfig emitOnSettle dMsg record.filterDebouncer
                    in
                    ( Open { record | filterDebouncer = filterDebouncer }, Cmd.map msgTagger cmd )

                AllClosed ->
                    ( AllClosed, Cmd.none )


filterDebouncerConfig : Debounce.Config (Msg msg)
filterDebouncerConfig =
    Debounce.Config (Debounce.later settleMillis) DebounceMsg


settleMillis : Float
settleMillis =
    500


emitOnSettle : Debounce.Send String (Msg msg)
emitOnSettle =
    Debounce.takeLast (emit << FilterSettle)


isOpen : String -> State -> Bool
isOpen id state =
    case state of
        Open record ->
            record.id == id

        AllClosed ->
            False


sub : (Msg msg -> msg) -> State -> Sub msg
sub msgTagger state =
    case state of
        Open { id, readyToClose } ->
            if readyToClose then
                let
                    closer =
                        msgTagger (Toggle id False)
                in
                Sub.batch
                    [ Browser.Events.onClick <|
                        let
                            targetIsNotSelectParts className =
                                List.all (\class_ -> not (String.contains class_ className))
                                    -- Other targets should stop propagation of click events on bubbling phase
                                    [ optionsClass, optionFilterClass ]
                        in
                        when (at [ "target", "className" ] string) targetIsNotSelectParts (succeed closer)
                    , Browser.Events.onKeyDown <|
                        when (field "key" string) ((==) "Escape") (succeed closer)
                    ]

            else
                -- This additional step is required due to a bug:
                -- https://discourse.elm-lang.org/t/mouse-clicks-subscription-created-and-executed-following-click-event/1067
                Browser.Events.onAnimationFrame <|
                    \_ -> msgTagger ReadyToClose

        AllClosed ->
            Sub.none



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
select : List (Attribute msg) -> Options a msg -> Html msg
select userAttrs opts =
    let
        opened =
            isOpen opts.id opts.state

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

        onHeaderPress =
            opts.msgTagger (Toggle opts.id (not opened))
    in
    div attrs
        [ header onHeaderPress opts
        , optionsWithFilter opened opts
        ]


header : msg -> Options a msg -> Html msg
header onPress opts =
    div
        [ class headerClass
        , Layout.flexRow
        , Layout.flexCenter
        , headerPadding opts.thin
        , Layout.spacingRow2
        , Border.round5
        , Background.colorNote
        , stopPropagationOn "click" (succeed ( onPress, True ))
        , onEnterKeyDown onPress
        , tabindex 0
        ]
        [ div [ class headerTextClass, Layout.flexGrow ]
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
        Layout.padding5


optionsWithFilter : Bool -> Options a msg -> Html msg
optionsWithFilter opened opts =
    if opened then
        div
            [ class optionsClass
            , Border.round5
            , Background.colorNote
            ]
            [ optionFilter opts
            , optionList opts
            ]

    else
        none


optionFilter : Options a msg -> Html msg
optionFilter opts =
    case opts.filterMatch of
        Just _ ->
            Html.input
                [ type_ "text"
                , class optionFilterClass
                , placeholder "Filter"
                , onInput (opts.msgTagger << FilterInput)
                , Layout.widthFill
                , Layout.padding2
                , Border.y1
                , Border.solid
                , Background.colorNote
                , value <|
                    case opts.state of
                        Open { filter } ->
                            filter

                        AllClosed ->
                            ""
                ]
                []

        Nothing ->
            none


optionList : Options a msg -> Html msg
optionList opts =
    Html.Keyed.node "div" [ class optionListClass, Layout.flexColumn ] <|
        List.map (optionRowKey opts) <|
            case ( opts.state, opts.filterMatch ) of
                ( Open { filterSettled }, Just matcher ) ->
                    if filterSettled /= "" then
                        List.filter (Tuple.second >> matcher filterSettled) opts.options

                    else
                        opts.options

                _ ->
                    opts.options


optionRowKey : Options a msg -> ( String, a ) -> ( String, Html msg )
optionRowKey opts ( optionKey, option ) =
    let
        onSelect =
            opts.msgTagger (Pick (opts.onSelect option))
    in
    Tuple.pair optionKey <|
        div
            [ class optionRowClass
            , Layout.flexItem
            , Layout.padding5
            , attribute "role" "option"
            , tabindex 0
            , stopPropagationOn "click" (succeed ( onSelect, True ))
            , onEnterKeyDown onSelect
            , if opts.selectedOption == Just option then
                class optionActiveClass

              else
                noAttr
            ]
            [ opts.optionHtml option ]



-- STYLES


styles : List Style
styles =
    [ s_ (c selectClass) [ ( "max-width", "50vw" ) ] -- Global maximum width for select
    , s_ (c headerClass) [ ( "cursor", "pointer" ) ]
    , s_ (c headerTextClass)
        [ ( "white-space", "nowrap" )
        , ( "overflow", "hidden" )
        , ( "text-overflow", "ellipsis" )
        , ( "padding-left", px headerTextPaddingX )
        , ( "padding-right", px headerTextPaddingX )
        , ( "min-width", "0" )
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
        , ( "max-height", px optionListMaxHeight )
        , ( "min-width", px optionListMinWidth )
        , ( "max-width", "50vw" )
        ]
    , s_ (c optionRowClass) [ ( "cursor", "pointer" ) ]
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
                List.map ((++) (c themeClass ++ " " ++ c optionRowClass)) <|
                    [ ":hover", ":focus" ]
      in
      s_ focusSelector [ ( "background-color", cssRgba theme.sub ) ]
    , s_ (c themeClass ++ " " ++ c optionActiveClass) [ ( "background-color", cssRgba theme.prim ) ]
    ]


selectClass : String
selectClass =
    "sl"


headerClass : String
headerClass =
    "slhead"


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


optionActiveClass : String
optionActiveClass =
    "slopac"
