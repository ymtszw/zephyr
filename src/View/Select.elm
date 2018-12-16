module View.Select exposing (Msg(..), State, init, isOpen, select, update)

import Data.ColorTheme exposing (ColorTheme)
import Debounce exposing (Debounce)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Octicons
import Task
import View.Parts exposing (..)


{-| Global state of select input elements.

Only one select input element can be open at a time within applicaiton.
Therefore you should only have one instance of this type in your application's model.

-}
type State
    = Open { id : String, filter : String, filterSettled : String, filterDebouncer : Debounce String }
    | AllClosed


init : State
init =
    AllClosed


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
            ( AllClosed, send x )

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
                            Debounce.update filterDebouncerConfig sendOnSettle dMsg record.filterDebouncer
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


sendOnSettle : Debounce.Send String (Msg msg)
sendOnSettle =
    Debounce.takeLast (send << FilterSettle)


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


isOpen : String -> State -> Bool
isOpen id state =
    case state of
        Open record ->
            record.id == id

        AllClosed ->
            False


type alias Options a msg =
    { state : State
    , msgTagger : Msg msg -> msg
    , id : String
    , theme : ColorTheme
    , thin : Bool
    , onSelect : a -> msg
    , selectedOption : Maybe a
    , filterMatch : Maybe (String -> a -> Bool)
    , options : List ( String, a )
    , optionEl : a -> Element msg
    }


{-| Select input element.

Require `id` and `state` to control open/closed status.
Also, it uess Keyed.column.

-}
select : List (Attribute msg) -> Options a msg -> Element msg
select userAttrs opts =
    let
        opened =
            isOpen opts.id opts.state

        attrs =
            [ below (optionsWithFilterEl opened opts) ] ++ userAttrs

        onHeaderPress =
            opts.msgTagger (Toggle opts.id (not opened))
    in
    el attrs (headerEl onHeaderPress opts)


headerEl : msg -> Options a msg -> Element msg
headerEl onPress opts =
    Element.Input.button
        [ width fill
        , Font.color opts.theme.text
        ]
        { onPress = Just onPress
        , label =
            row
                [ width fill
                , padding (headerPadding opts.thin)
                , spacingXY headerChevronSpacingX 0
                , BD.rounded rectElementRound
                , BG.color opts.theme.note
                ]
                [ -- `minimum 0` enforces `min-width: 0;` style which allows clip/scroll inside flex items
                  -- <http://kudakurage.hatenadiary.com/entry/2016/04/01/232722>
                  el [ width (fill |> minimum 0), paddingXY headerTextPaddingX 0, clipX ] <|
                    Maybe.withDefault (text "Select...") (Maybe.map opts.optionEl opts.selectedOption)
                , octiconEl
                    [ width (px headerChevronSize)
                    , alignRight
                    , BD.roundEach
                        { topLeft = 0
                        , topRight = rectElementRound
                        , bottomLeft = 0
                        , bottomRight = rectElementRound
                        }
                    , BG.color opts.theme.sub
                    ]
                    { size = headerChevronSize, color = defaultOcticonColor, shape = Octicons.chevronDown }
                ]
        }


headerPadding : Bool -> Int
headerPadding thin =
    if thin then
        0

    else
        5


headerTextPaddingX : Int
headerTextPaddingX =
    3


headerChevronSpacingX : Int
headerChevronSpacingX =
    3


headerChevronSize : Int
headerChevronSize =
    20


optionsWithFilterEl : Bool -> Options a msg -> Element msg
optionsWithFilterEl opened opts =
    column
        [ height (fill |> maximum optionListMaxHeight)
        , paddingXY 0 optionListPaddingY
        , visible opened
        , BD.rounded rectElementRound
        , BD.shadow
            { offset = ( 5.0, 5.0 )
            , blur = 10.0
            , size = 0.0
            , color = opts.theme.bg
            }
        , BG.color opts.theme.note
        ]
        [ lazy optionFilterEl opts
        , lazy optionsEl opts
        ]


optionFilterEl : Options a msg -> Element msg
optionFilterEl opts =
    case opts.filterMatch of
        Just _ ->
            textInputEl
                [ BD.rounded 0
                , BD.widthXY 0 1
                , BD.color opts.theme.bd
                ]
                { onChange = opts.msgTagger << FilterInput
                , theme = opts.theme
                , enabled = True
                , text =
                    case opts.state of
                        Open { filter } ->
                            filter

                        AllClosed ->
                            ""
                , label = Element.Input.labelHidden "Select Filter"
                , placeholder = Just (text "Filter")
                }

        Nothing ->
            none


optionsEl : Options a msg -> Element msg
optionsEl opts =
    Element.Keyed.column [ width (fill |> minimum optionListMinWidth), scrollbarY ] <|
        List.map (optionRowKeyEl opts) <|
            case ( opts.state, opts.filterMatch ) of
                ( Open { filterSettled }, Just matcher ) ->
                    if filterSettled /= "" then
                        List.filter (Tuple.second >> matcher filterSettled) opts.options

                    else
                        opts.options

                _ ->
                    opts.options


optionListMinWidth : Int
optionListMinWidth =
    100


optionListMaxHeight : Int
optionListMaxHeight =
    300


optionListPaddingY : Int
optionListPaddingY =
    5


optionRowKeyEl : Options a msg -> ( String, a ) -> ( String, Element msg )
optionRowKeyEl opts ( optionKey, option ) =
    let
        onPickOption =
            opts.msgTagger (Pick (opts.onSelect option))
    in
    Element.Input.button
        [ width fill
        , padding optionPadding
        , mouseOver [ BG.color opts.theme.sub ]
        , if opts.selectedOption == Just option then
            BG.color opts.theme.prim

          else
            noneAttr
        ]
        { onPress = Just onPickOption
        , label = opts.optionEl option
        }
        |> Tuple.pair optionKey


optionPadding : Int
optionPadding =
    5
