module View.Organisms.Modeless exposing
    ( State, ModelessId(..), idStr, init, push, remove, map
    , ResolvedPayload(..), render, styles
    )

{-| Modeless Window for dialog and other stuff.

It is Modeless in that it should not impede other user actions while it is shown.
Also it can be repositioned by drag and drop.

ModelessWindows are identified by unique String IDs.

@docs State, ModelessId, idStr, init, push, remove, map
@docs ResolvedPayload, render, styles

-}

import Data.Column exposing (ColumnItem)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Html.Keyed
import Json.Decode exposing (succeed)
import List.Extra
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Molecules.RawColumnItem as RawColumnItem
import View.Style exposing (..)


{-| Must be saved in Model.

Internal list indicates stacking order of Modeless Windows.

-}
type State
    = State (List ( ModelessId, Translate ))


type ModelessId
    = RawColumnItemId String Int


idStr : ModelessId -> String
idStr mId =
    case mId of
        RawColumnItemId columnId itemIndex ->
            "rawColumnItem_" ++ columnId ++ "_" ++ String.fromInt itemIndex


type alias Translate =
    { x : Int
    , y : Int
    }


init : State
init =
    State []


push : ModelessId -> State -> State
push id (State list) =
    case list of
        [] ->
            State [ ( id, Translate 0 0 ) ]

        ( _, { x, y } ) :: _ ->
            case List.Extra.find (\( id_, _ ) -> id_ == id) list of
                Just ( _, translate ) ->
                    let
                        dedup ( id_, _ ) =
                            case id_ of
                                RawColumnItemId columnId itemIndex ->
                                    ( columnId, itemIndex )
                    in
                    State <| List.Extra.uniqueBy dedup <| ( id, translate ) :: list

                Nothing ->
                    State (( id, Translate (x + staggerAmountOnPush) (y + staggerAmountOnPush) ) :: list)


staggerAmountOnPush : Int
staggerAmountOnPush =
    20


remove : ModelessId -> State -> State
remove id (State list) =
    let
        remover ( id_, _ ) =
            id_ /= id
    in
    State (List.filter remover list)


map : (ModelessId -> ResolvedPayload) -> State -> Props
map mapper (State list) =
    List.map (Tuple.mapFirst mapper) list


type alias Effects msg =
    { onCloseButtonClick : ModelessId -> msg
    , onAnywhereClick : ModelessId -> msg
    }


{-| Payload must be resolved at runtime from State.
-}
type alias Props =
    List ( ResolvedPayload, Translate )


type ResolvedPayload
    = RawColumnItem ModelessId ColumnItem


render : Effects msg -> Props -> Html msg
render eff props =
    let
        renderImpl ( resolved, translate_ ) =
            case resolved of
                RawColumnItem id columnItem ->
                    Tuple.pair (idStr id) <|
                        div
                            [ class modelessWindowClass
                            , padding5
                            , Background.colorMain
                            , Border.w1
                            , Border.solid
                            , translate translate_
                            , onClick (eff.onAnywhereClick id)
                            ]
                            [ div [ flexRow ]
                                [ t "[PH] Modeless Header"
                                , button
                                    [ flexItem
                                    , pushRight
                                    , padding5
                                    , Image.hovText
                                    , Background.transparent
                                    , Background.hovSub
                                    , Border.elliptic
                                    , stopPropagationOn "click" (succeed ( eff.onCloseButtonClick id, True ))
                                    ]
                                    [ Image.octicon { size = regularSize, shape = Octicons.x }
                                    ]
                                ]
                            , RawColumnItem.render columnItem
                            ]

        translate { x, y } =
            case ( x, y ) of
                ( 0, 0 ) ->
                    noAttr

                ( 0, _ ) ->
                    style "transform" ("translateY(" ++ px y ++ ")")

                ( _, 0 ) ->
                    style "transform" ("translateX(" ++ px x ++ ")")

                ( _, _ ) ->
                    style "transform" ("translate(" ++ px x ++ "," ++ px y ++ ")")
    in
    Html.Keyed.node "div" [] <|
        -- Reverse, since items at head must be shown at the end (stacked on the top)
        List.Extra.reverseMap renderImpl props


styles : List Style
styles =
    [ s (c modelessWindowClass)
        [ ( "position", "fixed" )
        , ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        , ( "margin-top", "auto" )
        , ( "margin-bottom", "auto" )
        , ( "max-width", "90vw" )
        , ( "max-height", "90vh" )
        , ( "top", "50px" )
        , ( "left", "50px" )
        ]
    ]


modelessWindowClass : String
modelessWindowClass =
    "mw"
