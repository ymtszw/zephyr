module View.Select exposing (State, close, el, init, isOpen, open)

import Data.ColorTheme exposing (oneDark)
import Data.Msg exposing (Msg(..))
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Input
import Extra exposing (ite)
import Octicons
import Set exposing (Set)
import View.Parts exposing (octiconEl)


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
    El.row [ El.width El.fill ]
        [ El.el
            [ El.width El.fill
            , El.below (ite (isOpen id state) (optionsEl onSelectMsg noMsgOptionEl options) El.none)
            ]
            (headerEl (SelectToggle id (not (isOpen id state))) selectedOption noMsgOptionEl)
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
                , El.el [ El.alignRight, BG.color oneDark.sub ] (octiconEl Octicons.chevronDown)
                ]
        }


optionsEl : (a -> Msg) -> (a -> Element Msg) -> List a -> Element Msg
optionsEl onSelectMsg noMsgOptionEl options =
    List.map noMsgOptionEl options
        |> El.column
            [ El.width El.fill
            , El.paddingXY 10 3
            , BG.color oneDark.note
            ]
