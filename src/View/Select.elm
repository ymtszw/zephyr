module View.Select exposing (State, close, init, isOpen, open, select)

import Data.ColorTheme exposing (oneDark)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy3, lazy4)
import Extra exposing (ite)
import Octicons
import Set exposing (Set)
import View.Parts exposing (octiconFreeSizeEl, scale12)


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
    , onSelect : a -> Msg
    , selectedOption : Maybe a
    , noMsgOptionEl : a -> Element Msg
    }
    -> State
    -> List ( String, a )
    -> Element Msg
select { id, onSelect, selectedOption, noMsgOptionEl } state options =
    let
        opened =
            isOpen id state
    in
    el
        [ width (fill |> minimum 0)
        , below (ite opened (optionsEl onSelect noMsgOptionEl selectedOption options) none)
        , Font.size (scale12 2)
        ]
        (lazy3 headerEl (SelectToggle id (not opened)) selectedOption noMsgOptionEl)


headerEl : Msg -> Maybe a -> (a -> Element Msg) -> Element Msg
headerEl onPress selectedOption noMsgOptionEl =
    Element.Input.button
        [ width fill
        , spacing 3
        , padding 5
        , BD.rounded 5
        , BG.color oneDark.note
        ]
        { onPress = Just onPress
        , label =
            row [ width fill ]
                [ -- `minimum 0` enforces `min-width: 0;` style which allows clip/scroll inside flex items
                  -- <http://kudakurage.hatenadiary.com/entry/2016/04/01/232722>
                  el [ width (fill |> minimum 0), clipX ] <|
                    Maybe.withDefault (text "Select...") (Maybe.map noMsgOptionEl selectedOption)
                , el [ width (px 20), alignRight, BG.color oneDark.sub ] <|
                    octiconFreeSizeEl 20 Octicons.chevronDown
                ]
        }


optionsEl : (a -> Msg) -> (a -> Element Msg) -> Maybe a -> List ( String, a ) -> Element Msg
optionsEl onSelect noMsgOptionEl selectedOption options =
    options
        |> List.map (optionEl onSelect noMsgOptionEl selectedOption)
        |> Element.Keyed.column
            [ width (fill |> minimum 100)
            , paddingXY 0 5
            , scrollbarY
            , BD.width 1
            , BD.rounded 5
            , BD.color oneDark.bd
            , BD.shadow
                { offset = ( 5.0, 5.0 )
                , blur = 10.0
                , size = 0.0
                , color = oneDark.bg
                }
            , BG.color oneDark.note
            ]
        |> el [ height (fill |> maximum 300) ]


optionEl : (a -> Msg) -> (a -> Element Msg) -> Maybe a -> ( String, a ) -> ( String, Element Msg )
optionEl onSelect noMsgOptionEl selectedOption ( optionKey, option ) =
    let
        selectedStyle =
            ite (selectedOption == Just option) [ BG.color oneDark.active ] []
    in
    Element.Input.button
        (selectedStyle
            ++ [ width fill
               , padding 5
               , mouseOver [ BG.color oneDark.sub ]
               ]
        )
        { onPress = Just (SelectPick (onSelect option))
        , label = noMsgOptionEl option
        }
        |> Tuple.pair optionKey
