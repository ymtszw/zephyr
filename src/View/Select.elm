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
    List (Attribute Msg)
    ->
        { state : State
        , id : String
        , theme : ColorTheme
        , onSelect : a -> Msg
        , selectedOption : Maybe a
        , options : List ( String, a )
        , optionEl : a -> Element Msg
        }
    -> Element Msg
select userAttrs { state, id, theme, onSelect, selectedOption, options, optionEl } =
    let
        opened =
            isOpen id state

        attrs =
            [ width (fill |> minimum 0)
            , height fill
            , padding headerPadding
            , BD.rounded rectElementRound
            , BG.color theme.note
            , below (optionsEl onSelect theme opened optionEl selectedOption options)
            ]
                ++ userAttrs
    in
    el attrs (headerEl (SelectToggle id (not opened)) theme selectedOption optionEl)


headerEl : Msg -> ColorTheme -> Maybe a -> (a -> Element Msg) -> Element Msg
headerEl onPress theme selectedOption optionEl =
    Element.Input.button
        [ width fill
        , Font.color theme.text
        ]
        { onPress = Just onPress
        , label =
            row [ width fill, spacingXY headerChevronSpacingX 0 ]
                [ -- `minimum 0` enforces `min-width: 0;` style which allows clip/scroll inside flex items
                  -- <http://kudakurage.hatenadiary.com/entry/2016/04/01/232722>
                  el [ width (fill |> minimum 0), clipX ] <|
                    Maybe.withDefault (text "Select...") (Maybe.map optionEl selectedOption)
                , octiconEl
                    [ width (px headerChevronSize)
                    , alignRight
                    , BD.roundEach
                        { topLeft = 0
                        , topRight = rectElementRound
                        , bottomLeft = 0
                        , bottomRight = rectElementRound
                        }
                    , BG.color theme.sub
                    ]
                    { size = headerChevronSize, color = defaultOcticonColor, shape = Octicons.chevronDown }
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


optionsEl : (a -> Msg) -> ColorTheme -> Bool -> (a -> Element Msg) -> Maybe a -> List ( String, a ) -> Element Msg
optionsEl onSelect theme opened optionEl selectedOption options =
    options
        |> List.map (optionRowKeyEl onSelect theme optionEl selectedOption)
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
        |> el [ height (fill |> maximum optionListMaxHeight), visible opened ]


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


optionRowKeyEl : (a -> Msg) -> ColorTheme -> (a -> Element Msg) -> Maybe a -> ( String, a ) -> ( String, Element Msg )
optionRowKeyEl onSelect theme optionEl selectedOption ( optionKey, option ) =
    Element.Input.button
        [ width fill
        , padding optionPadding
        , mouseOver [ BG.color theme.sub ]
        , if selectedOption == Just option then
            BG.color theme.prim

          else
            noneAttr
        ]
        { onPress = Just (SelectPick (onSelect option))
        , label = optionEl option
        }
        |> Tuple.pair optionKey


optionPadding : Int
optionPadding =
    5
