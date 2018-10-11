module View.Select exposing (State, close, el, init, isOpen, open)

import Data.ColorTheme exposing (oneDark)
import Data.Msg exposing (Msg(..))
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Extra exposing (ite)
import Html.Attributes
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

-}
el :
    { id : String
    , onSelect : a -> Msg
    , selectedOption : Maybe a
    , noMsgOptionEl : a -> Element Msg
    }
    -> State
    -> List a
    -> Element Msg
el { id, onSelect, selectedOption, noMsgOptionEl } state options =
    let
        opened =
            isOpen id state
    in
    -- `El.minimum 0` enforces `min-width: 0;` style which allows clip/scroll inside flex items
    -- <http://kudakurage.hatenadiary.com/entry/2016/04/01/232722>
    El.row [ El.width (El.fill |> El.minimum 0), Font.size (scale12 2) ]
        [ El.el
            [ El.width (El.fill |> El.minimum 0)
            , El.below (ite opened (optionsEl onSelect noMsgOptionEl selectedOption options) El.none)
            ]
            (headerEl (SelectToggle id (not opened)) selectedOption noMsgOptionEl)
        ]


headerEl : Msg -> Maybe a -> (a -> Element Msg) -> Element Msg
headerEl onPress selectedOption noMsgOptionEl =
    Element.Input.button
        [ El.width (El.fill |> El.minimum 0)
        , BD.rounded 5
        , El.padding 5
        , BG.color oneDark.note
        ]
        { onPress = Just onPress
        , label =
            El.row [ El.width (El.fill |> El.minimum 0), El.spacing 3 ]
                [ El.el [ El.width (El.fill |> El.minimum 0), El.clipX ] <|
                    Maybe.withDefault (El.text "Select...") (Maybe.map noMsgOptionEl selectedOption)
                , El.el [ El.width (El.px 20), El.alignRight, BG.color oneDark.sub ] <|
                    octiconFreeSizeEl 20 Octicons.chevronDown
                ]
        }


optionsEl : (a -> Msg) -> (a -> Element Msg) -> Maybe a -> List a -> Element Msg
optionsEl onSelect noMsgOptionEl selectedOption options =
    options
        |> List.map (optionEl onSelect noMsgOptionEl selectedOption)
        |> El.column
            [ El.width (El.shrink |> El.minimum 0)
            , El.height (El.fill |> El.minimum 0 |> El.maximum 300)
            , El.scrollbarY
            , El.clipX
            , El.paddingXY 0 5
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


optionEl : (a -> Msg) -> (a -> Element Msg) -> Maybe a -> a -> Element Msg
optionEl onSelect noMsgOptionEl selectedOption option =
    let
        selectedStyle =
            ite (selectedOption == Just option) [ BG.color oneDark.active ] []
    in
    El.el
        (selectedStyle
            ++ [ El.width (El.fill |> El.minimum 80)
               , El.padding 5
               , El.mouseOver [ BG.color oneDark.sub ]
               , El.pointer
               , Element.Events.onClick (SelectPick (onSelect option))
               ]
        )
        (noMsgOptionEl option)
