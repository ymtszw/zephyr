module View.Select exposing (State, close, el, init, isOpen, open)

import Data.ColorTheme exposing (oneDark)
import Data.Msg exposing (Msg(..))
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
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

-}
el :
    { id : String
    , onSelectMsg : a -> Msg
    , selectedOption : Maybe a
    , noMsgOptionEl : a -> Element Msg
    }
    -> State
    -> List a
    -> Element Msg
el { id, onSelectMsg, selectedOption, noMsgOptionEl } state options =
    let
        opened =
            isOpen id state
    in
    El.row [ El.width El.fill, Font.size (scale12 2) ]
        [ El.el
            [ El.width El.fill
            , El.below (ite opened (optionsEl onSelectMsg noMsgOptionEl options) El.none)
            ]
            (headerEl (SelectToggle id (not opened)) selectedOption noMsgOptionEl)
        ]


headerEl : Msg -> Maybe a -> (a -> Element Msg) -> Element Msg
headerEl onPress selectedOption noMsgOptionEl =
    Element.Input.button
        [ El.width El.fill
        , BD.rounded 5
        , El.padding 5
        , BG.color oneDark.note
        ]
        { onPress = Just onPress
        , label =
            El.row [ El.width El.fill ]
                [ Maybe.withDefault (El.text "Select...") (Maybe.map noMsgOptionEl selectedOption)
                , El.el [ El.alignRight, BG.color oneDark.sub ] (octiconFreeSizeEl 20 Octicons.chevronDown)
                ]
        }


optionsEl : (a -> Msg) -> (a -> Element Msg) -> List a -> Element Msg
optionsEl onSelectMsg noMsgOptionEl options =
    options
        |> List.map
            (\option ->
                Element.Input.button
                    [ El.width El.fill
                    , El.padding 5
                    , El.mouseOver [ BG.color oneDark.sub ]
                    ]
                    { onPress = Just (SelectPick (onSelectMsg option))
                    , label = El.el [ El.alignLeft ] (noMsgOptionEl option)
                    }
            )
        |> El.column
            [ El.width El.fill
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
