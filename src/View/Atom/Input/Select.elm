module View.Atom.Input.Select exposing
    ( State(..), Msg(..), update, isOpen
    , Options, select, styles
    )

{-| Select Input Atom.

Since this select input is implemented in pure Elm,
it is a component with State and Msg, in order to control its toggle state,
and filtering feature.

@docs State, Msg, update, isOpen
@docs Options, select, styles

-}

import Color exposing (cssRgba)
import Debounce exposing (Debounce)
import Extra exposing (emit)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    = Open { id : String, filter : String, filterSettled : String, filterDebouncer : Debounce String }
    | AllClosed



-- UPDATE


type Msg msg
    = Toggle String Bool
    | Pick msg
    | FilterInput String
    | FilterSettle String
    | DebounceMsg Debounce.Msg


update : (Msg msg -> msg) -> Msg msg -> State -> ( State, Cmd msg )
update msgTagger msg state =
    case msg of
        Toggle id True ->
            ( Open { id = id, filter = "", filterSettled = "", filterDebouncer = Debounce.init }, Cmd.none )

        Toggle _ False ->
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
Also, it uess Keyed.column.

-}
select : List (Attribute msg) -> Options a msg -> Html msg
select userAttrs opts =
    let
        opened =
            isOpen opts.id opts.state

        attrs =
            [ class selectClass
            , attribute "role" "select"
            , tabindex 0
            ]
                ++ userAttrs

        onHeaderPress =
            opts.msgTagger (Toggle opts.id (not opened))
    in
    div attrs [ header onHeaderPress opts ]


header : msg -> Options a msg -> Html msg
header onPress opts =
    div
        [ Layout.flexRow
        , Layout.flexCenter
        , headerPadding opts.thin
        , Layout.spacingRow2
        , Border.round5
        , Background.colorNote
        ]
        [ div [ class headerTextClass, Layout.flexGrow ]
            [ Maybe.withDefault (text "Select...") (Maybe.map opts.optionHtml opts.selectedOption) ]
        , div [ class chevronClass, Border.rightRound5, Background.colorSub ]
            [ Image.octicon { size = headerChevronSize, shape = Octicons.chevronDown } ]
        ]


headerPadding : Bool -> Attribute msg
headerPadding thin =
    if thin then
        noAttr

    else
        Layout.padding5


headerChevronSize : Int
headerChevronSize =
    20



--
-- optionsWithFilterEl : Bool -> Options a msg -> Element msg
-- optionsWithFilterEl opened opts =
--     column
--         [ height (fill |> maximum optionListMaxHeight)
--         , paddingXY 0 optionListPaddingY
--         , visible opened
--         , BD.rounded rectElementRound
--         , BD.shadow
--             { offset = ( 5.0, 5.0 )
--             , blur = 10.0
--             , size = 0.0
--             , color = opts.theme.bg
--             }
--         , BG.color opts.theme.note
--         ]
--         [ lazy optionFilterEl opts
--         , lazy optionsEl opts
--         ]
--
--
-- optionFilterEl : Options a msg -> Element msg
-- optionFilterEl opts =
--     case opts.filterMatch of
--         Just _ ->
--             textInputEl
--                 [ BD.rounded 0
--                 , BD.widthXY 0 1
--                 , BD.color opts.theme.bd
--                 ]
--                 { onChange = opts.msgTagger << FilterInput
--                 , theme = opts.theme
--                 , enabled = True
--                 , text =
--                     case opts.state of
--                         Open { filter } ->
--                             filter
--
--                         AllClosed ->
--                             ""
--                 , label = Element.Input.labelHidden "Select Filter"
--                 , placeholder = Just (text "Filter")
--                 }
--
--         Nothing ->
--             none
--
--
-- optionsEl : Options a msg -> Element msg
-- optionsEl opts =
--     Element.Keyed.column [ width (fill |> minimum optionListMinWidth), scrollbarY ] <|
--         List.map (optionRowKeyEl opts) <|
--             case ( opts.state, opts.filterMatch ) of
--                 ( Open { filterSettled }, Just matcher ) ->
--                     if filterSettled /= "" then
--                         List.filter (Tuple.second >> matcher filterSettled) opts.options
--
--                     else
--                         opts.options
--
--                 _ ->
--                     opts.options
--
--
-- optionListMinWidth : Int
-- optionListMinWidth =
--     100
--
--
-- optionListMaxHeight : Int
-- optionListMaxHeight =
--     300
--
--
-- optionListPaddingY : Int
-- optionListPaddingY =
--     5
--
--
-- optionRowKeyEl : Options a msg -> ( String, a ) -> ( String, Element msg )
-- optionRowKeyEl opts ( optionKey, option ) =
--     let
--         onPickOption =
--             opts.msgTagger (Pick (opts.onSelect option))
--     in
--     Element.Input.button
--         [ width fill
--         , padding optionPadding
--         , mouseOver [ BG.color opts.theme.sub ]
--         , if opts.selectedOption == Just option then
--             BG.color opts.theme.prim
--
--           else
--             noneAttr
--         ]
--         { onPress = Just onPickOption
--         , label = opts.optionEl option
--         }
--         |> Tuple.pair optionKey
--
--
-- optionPadding : Int
-- optionPadding =
--     5
-- STYLES


styles : List Style
styles =
    [ s_ (c selectClass) [ ( "cursor", "pointer" ) ]
    , s_ (c headerTextClass)
        [ ( "padding-left", px headerTextPaddingX )
        , ( "padding-right", px headerTextPaddingX )
        ]
    , s_ (c chevronClass)
        [ ( "width", px headerChevronSize )
        , ( "height", px headerChevronSize )
        ]
    ]
        ++ themedStyles oneDarkClass oneDarkTheme
        ++ themedStyles aubergineClass aubergineTheme


s_ =
    View.Style.s


themedStyles : String -> Theme -> List Style
themedStyles themeClass theme =
    [ scoped (c themeClass) (c selectClass) [ ( "color", cssRgba theme.text ) ]
    ]


headerTextPaddingX : Int
headerTextPaddingX =
    3


selectClass : String
selectClass =
    "sl"


headerTextClass : String
headerTextClass =
    "slhtxt"


chevronClass : String
chevronClass =
    "slchev"
