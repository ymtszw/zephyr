module View.Organisms.Modeless exposing
    ( State, ModelessId(..), idStr, init, sub, touch, move, remove, map
    , ResolvedPayload(..), render, styles
    )

{-| Modeless Window for dialog and other stuff.

It is Modeless in that it should not impede other user actions while it is shown.
Also it can be repositioned by drag and drop.

ModelessWindows are identified by unique String IDs.

@docs State, ModelessId, idStr, init, sub, touch, move, remove, map
@docs ResolvedPayload, render, styles

-}

import Browser.Events
import Data.Column exposing (ColumnItem)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, draggable, style)
import Html.Events exposing (on, onClick, preventDefaultOn, stopPropagationOn)
import Html.Keyed
import Json.Decode exposing (..)
import Json.DecodeExtra exposing (do)
import List.Extra
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (nowrap)
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
    , prevDragCoord : Maybe ( Int, Int )
    }


init : State
init =
    State []


sub : (ModelessId -> Int -> Int -> msg) -> State -> Sub msg
sub toMsg (State list) =
    let
        dragged ( _, { prevDragCoord } ) =
            prevDragCoord /= Nothing
    in
    case List.Extra.find dragged list of
        Just ( id, _ ) ->
            Browser.Events.onMouseMove (cursorCoordDecoder (toMsg id))

        Nothing ->
            Sub.none


cursorCoordDecoder : (Int -> Int -> msg) -> Decoder msg
cursorCoordDecoder toMsg =
    do (field "clientX" int) <|
        \cx ->
            do (field "clientY" int) <|
                \cy ->
                    if cx == 0 && cy == 0 then
                        -- Outlier case happens just before dragend
                        fail "0,0"

                    else
                        succeed (toMsg cx cy)


touch : ModelessId -> State -> State
touch id (State list) =
    case list of
        [] ->
            State [ ( id, Translate 0 0 Nothing ) ]

        ( _, { x, y } ) :: _ ->
            case List.Extra.find (\( id_, _ ) -> id_ == id) list of
                Just ( _, trans ) ->
                    -- Quit drag, can be used on DragEnd
                    State (consDedup ( id, { trans | prevDragCoord = Nothing } ) list)

                Nothing ->
                    -- Since the subject is not found, no need to dedup
                    if x == 0 && y == 0 then
                        State (( id, Translate staggerAmountOnPush staggerAmountOnPush Nothing ) :: list)

                    else
                        State (( id, Translate 0 0 Nothing ) :: list)


consDedup : ( ModelessId, Translate ) -> List ( ModelessId, Translate ) -> List ( ModelessId, Translate )
consDedup a list =
    let
        dedup ( id, _ ) =
            case id of
                RawColumnItemId columnId itemIndex ->
                    ( columnId, itemIndex )
    in
    List.Extra.uniqueBy dedup (a :: list)


staggerAmountOnPush : Int
staggerAmountOnPush =
    20


move : ( ModelessId, Int, Int ) -> State -> State
move ( id, newX, newY ) (State list) =
    case List.Extra.find (\( id_, _ ) -> id_ == id) list of
        Just ( _, trans ) ->
            case trans.prevDragCoord of
                Just ( prevX, prevY ) ->
                    -- Already moving
                    let
                        newTrans =
                            { x = trans.x + newX - prevX
                            , y = trans.y + newY - prevY
                            , prevDragCoord = Just ( newX, newY )
                            }
                    in
                    State (consDedup ( id, newTrans ) list)

                Nothing ->
                    -- Start moving
                    State (consDedup ( id, { trans | prevDragCoord = Just ( newX, newY ) } ) list)

        Nothing ->
            -- Not found but dragged? Curious...
            State (( id, Translate 0 0 (Just ( newX, newY )) ) :: list)


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



-- VIEW


type alias Effects msg =
    { onCloseButtonClick : ModelessId -> msg
    , onAnywhereClick : ModelessId -> msg
    , onDrag : ModelessId -> Int -> Int -> msg
    , onDragEnd : ModelessId -> msg
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
        renderImpl ( resolved, trans ) =
            case resolved of
                RawColumnItem id columnItem ->
                    withHeader trans id "Source of Column Item" (RawColumnItem.render columnItem)

        withHeader trans id title content =
            let
                header =
                    let
                        dragHandlers =
                            case trans.prevDragCoord of
                                Just _ ->
                                    [ on "mouseup" (succeed (eff.onDragEnd id)) ]

                                Nothing ->
                                    [ on "mousedown" (cursorCoordDecoder (eff.onDrag id)) ]
                    in
                    div [ class headerClass, flexRow ]
                        [ div [ nowrap, padding5, Background.colorBg, Border.topRound5 ] [ t title ]
                        , div ([ flexGrow, Cursor.allScroll ] ++ dragHandlers) []
                        , button
                            [ flexItem
                            , alignStart
                            , padding2
                            , Image.fillText
                            , Background.transparent
                            , Background.hovSub
                            , Border.elliptic
                            , stopPropagationOn "click" (succeed ( eff.onCloseButtonClick id, True ))
                            ]
                            [ Image.octicon { size = prominentSize, shape = Octicons.x }
                            ]
                        ]
            in
            Tuple.pair (idStr id) <|
                div
                    [ class modelessWindowClass
                    , flexColumn
                    , padding2
                    , Background.colorNote
                    , Border.round5
                    , translate trans
                    , onClick (eff.onAnywhereClick id)
                    ]
                    [ header
                    , content
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
        , ( "max-width", "90vw" )
        , ( "max-height", "90vh" )
        , ( "top", "50px" )
        , ( "left", "50px" )
        ]
    , s (c headerClass)
        [ ( "padding-left", "10px" ) ]
    ]


modelessWindowClass : String
modelessWindowClass =
    "mw"


headerClass : String
headerClass =
    "mwhd"
