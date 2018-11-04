module View.Select exposing (State, close, init, isOpen, open, select)

import Data.ColorTheme exposing (ColorTheme)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Extra exposing (ite)
import Octicons
import View.Parts exposing (..)


{-| Global state of select input elements.

Only one select input element can be open at a time within applicaiton.
Therefore you should only have one instance of this type in your application's model.

-}
type State
    = Open String
    | AllClosed


init : State
init =
    AllClosed


open : String -> State -> State
open id state =
    Open id


close : State
close =
    AllClosed


isOpen : String -> State -> Bool
isOpen id state =
    case state of
        Open openId ->
            openId == id

        AllClosed ->
            False


{-| Select input element.

Require `id` and `state` to control open/closed status.
Also, it uess Keyed.column.

-}
select :
    { id : String
    , theme : ColorTheme
    , onSelect : a -> Msg
    , selectedOption : Maybe a
    , noMsgOptionEl : a -> Element Msg
    }
    -> State
    -> List ( String, a )
    -> Element Msg
select { id, theme, onSelect, selectedOption, noMsgOptionEl } state options =
    let
        opened =
            isOpen id state
    in
    el
        [ width (fill |> minimum 0)
        , height fill
        , ite opened (below (optionsEl onSelect theme noMsgOptionEl selectedOption options)) noneAttr
        ]
        (headerEl (SelectToggle id (not opened)) theme selectedOption noMsgOptionEl)


headerEl : Msg -> ColorTheme -> Maybe a -> (a -> Element Msg) -> Element Msg
headerEl onPress theme selectedOption noMsgOptionEl =
    Element.Input.button
        [ width fill
        , padding headerPadding
        , BD.rounded rectElementRound
        , BG.color theme.note
        , Font.color theme.text
        ]
        { onPress = Just onPress
        , label =
            row [ width fill, spacingXY headerChevronSpacingX 0 ]
                [ -- `minimum 0` enforces `min-width: 0;` style which allows clip/scroll inside flex items
                  -- <http://kudakurage.hatenadiary.com/entry/2016/04/01/232722>
                  el [ width (fill |> minimum 0), clipX ] <|
                    Maybe.withDefault (text "Select...") (Maybe.map noMsgOptionEl selectedOption)
                , el [ width (px headerChevronSize), alignRight, BG.color theme.sub ] <|
                    octiconEl { size = headerChevronSize, color = defaultOcticonColor, shape = Octicons.chevronDown }
                ]
        }


headerPadding : Int
headerPadding =
    5


headerChevronSpacingX : Int
headerChevronSpacingX =
    3


headerChevronSize : Int
headerChevronSize =
    20


optionsEl : (a -> Msg) -> ColorTheme -> (a -> Element Msg) -> Maybe a -> List ( String, a ) -> Element Msg
optionsEl onSelect theme noMsgOptionEl selectedOption options =
    options
        |> List.map (optionEl onSelect theme noMsgOptionEl selectedOption)
        |> Element.Keyed.column
            [ width (fill |> minimum optionListMinWidth)
            , paddingXY 0 optionListPaddingY
            , scrollbarY
            , BD.width optionListBorderWidth
            , BD.rounded rectElementRound
            , BD.color theme.bd
            , BD.shadow
                { offset = ( 5.0, 5.0 )
                , blur = 10.0
                , size = 0.0
                , color = theme.bg
                }
            , BG.color theme.note
            ]
        |> el [ height (fill |> maximum optionListMaxHeight) ]


optionListMinWidth : Int
optionListMinWidth =
    100


optionListMaxHeight : Int
optionListMaxHeight =
    300


optionListPaddingY : Int
optionListPaddingY =
    5


optionListBorderWidth : Int
optionListBorderWidth =
    1


optionEl : (a -> Msg) -> ColorTheme -> (a -> Element Msg) -> Maybe a -> ( String, a ) -> ( String, Element Msg )
optionEl onSelect theme noMsgOptionEl selectedOption ( optionKey, option ) =
    Element.Input.button
        [ width fill
        , padding optionPadding
        , mouseOver [ BG.color theme.sub ]
        , ite (selectedOption == Just option) (BG.color theme.prim) noneAttr
        ]
        { onPress = Just (SelectPick (onSelect option))
        , label = noMsgOptionEl option
        }
        |> Tuple.pair optionKey


optionPadding : Int
optionPadding =
    5