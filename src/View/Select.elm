module View.Select exposing (State, close, filterInput, init, isOpen, open, select)

import Data.ColorTheme exposing (ColorTheme)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Octicons
import View.Parts exposing (..)


{-| Global state of select input elements.

Only one select input element can be open at a time within applicaiton.
Therefore you should only have one instance of this type in your application's model.

-}
type State
    = Open { id : String, filter : String }
    | AllClosed


init : State
init =
    AllClosed


open : String -> State -> State
open id state =
    Open { id = id, filter = "" }


filterInput : String -> State -> State
filterInput filter state =
    case state of
        Open record ->
            Open { record | filter = filter }

        AllClosed ->
            AllClosed


close : State
close =
    AllClosed


isOpen : String -> State -> Bool
isOpen id state =
    case state of
        Open record ->
            record.id == id

        AllClosed ->
            False


type alias Options a =
    { state : State
    , id : String
    , theme : ColorTheme
    , thin : Bool
    , onSelect : a -> Msg
    , selectedOption : Maybe a
    , filterMatch : Maybe (String -> a -> Bool)
    , options : List ( String, a )
    , optionEl : a -> Element Msg
    }


{-| Select input element.

Require `id` and `state` to control open/closed status.
Also, it uess Keyed.column.

-}
select : List (Attribute Msg) -> Options a -> Element Msg
select userAttrs opts =
    let
        opened =
            isOpen opts.id opts.state

        attrs =
            [ below (optionsWithFilterEl opened opts) ] ++ userAttrs
    in
    el attrs (headerEl (SelectToggle opts.id (not opened)) opts)


headerEl : Msg -> Options a -> Element Msg
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


optionsWithFilterEl : Bool -> Options a -> Element Msg
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


optionFilterEl : Options a -> Element Msg
optionFilterEl opts =
    case opts.filterMatch of
        Just _ ->
            textInputEl
                [ BD.rounded 0
                , BD.widthXY 0 1
                , BD.color opts.theme.bd
                ]
                { onChange = SelectFilterInput
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


optionsEl : Options a -> Element Msg
optionsEl opts =
    Element.Keyed.column [ width (fill |> minimum optionListMinWidth), scrollbarY ] <|
        List.map (optionRowKeyEl opts) <|
            case ( opts.state, opts.filterMatch ) of
                ( Open { filter }, Just matcher ) ->
                    List.filter (Tuple.second >> matcher filter) opts.options

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


optionRowKeyEl : Options a -> ( String, a ) -> ( String, Element Msg )
optionRowKeyEl opts ( optionKey, option ) =
    Element.Input.button
        [ width fill
        , padding optionPadding
        , mouseOver [ BG.color opts.theme.sub ]
        , if opts.selectedOption == Just option then
            BG.color opts.theme.prim

          else
            noneAttr
        ]
        { onPress = Just (SelectPick (opts.onSelect option))
        , label = opts.optionEl option
        }
        |> Tuple.pair optionKey


optionPadding : Int
optionPadding =
    5
