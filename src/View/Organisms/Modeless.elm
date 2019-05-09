module View.Organisms.Modeless exposing
    ( State, ModelessId(..), MediaViewrIdPayload, Msg(..), idStr, init, sub, update, map
    , Effects, Props, ResolvedPayload(..), render, styles
    )

{-| Modeless Window for dialog and other stuff.

It is Modeless in that it should not impede other user actions while it is shown.
Also it can be repositioned by drag and drop.

ModelessWindows are identified by unique String IDs.

@docs State, ModelessId, MediaViewrIdPayload, Msg, idStr, init, sub, update, map
@docs Effects, Props, ResolvedPayload, render, styles

-}

import Browser.Events
import Data.Column as Column exposing (ColumnItem)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on, onClick, stopPropagationOn)
import Html.Keyed
import Id
import Json.Decode exposing (..)
import List.Extra
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (nowrap)
import View.Atoms.Theme exposing (oneDark)
import View.Atoms.Typography exposing (..)
import View.Molecules.MediaViewer as MediaViewer
import View.Molecules.RawColumnItem as RawColumnItem
import View.Style exposing (..)


{-| Must be saved in Model.

Internal list indicates stacking order of Modeless Windows.

-}
type State
    = State (List ( ModelessId, Translate ))


type ModelessId
    = RawColumnItemId Column.Id Int
    | MediaViewerId MediaViewrIdPayload


type alias MediaViewrIdPayload =
    { columnId : Column.Id
    , itemIndex : Int
    , mediaIndex : Int
    , -- Belows are more of view states, not particularly "Id"
      isShrunk : Bool
    }


idStr : ModelessId -> String
idStr mId =
    case mId of
        RawColumnItemId columnId itemIndex ->
            "rawColumnItem_" ++ Id.to columnId ++ "_" ++ String.fromInt itemIndex

        MediaViewerId { columnId, itemIndex } ->
            "mediaViewer_" ++ Id.to columnId ++ "_" ++ String.fromInt itemIndex


type alias Translate =
    { x : Int
    , y : Int
    , prevDragCoord : Maybe ( Int, Int )
    }


init : State
init =
    State []


sub : State -> Sub Msg
sub (State list) =
    let
        dragged ( _, { prevDragCoord } ) =
            prevDragCoord /= Nothing
    in
    case List.Extra.find dragged list of
        Just ( id, _ ) ->
            Browser.Events.onMouseMove (cursorCoordDecoder (Move id))

        Nothing ->
            Sub.none


cursorCoordDecoder : (Int -> Int -> msg) -> Decoder msg
cursorCoordDecoder toMsg =
    map2 toMsg (field "clientX" int) (field "clientY" int)


type Msg
    = Touch ModelessId
    | Move ModelessId Int Int
    | Remove ModelessId
    | MediaViewerSelectAt ModelessId Int
    | MediaViewerToggleSize ModelessId Bool


{-| Unlike usual "update" function, this update is un-effectful, i.e. pure.

It is this way just in order to consolidate APIs into single endpoint.

-}
update : Msg -> State -> State
update msg (State list) =
    case msg of
        Touch id ->
            touch id (State list)

        Move id x y ->
            move ( id, x, y ) (State list)

        Remove id ->
            let
                remover ( id_, _ ) =
                    id_ /= id
            in
            State (List.filter remover list)

        MediaViewerSelectAt (RawColumnItemId _ _) _ ->
            State list

        MediaViewerSelectAt (MediaViewerId payload) newIndex ->
            touch (MediaViewerId { payload | mediaIndex = newIndex }) (State list)

        MediaViewerToggleSize (RawColumnItemId _ _) _ ->
            State list

        MediaViewerToggleSize (MediaViewerId payload) isShrunk ->
            touch (MediaViewerId { payload | isShrunk = isShrunk }) (State list)


touch : ModelessId -> State -> State
touch id (State list) =
    case list of
        [] ->
            State [ ( id, Translate 0 0 Nothing ) ]

        ( _, { x, y } ) :: _ ->
            case List.Extra.find (\( id_, _ ) -> idStr id_ == idStr id) list of
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
    List.Extra.uniqueBy (\( id, _ ) -> idStr id) (a :: list)


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


map : (ModelessId -> ResolvedPayload) -> State -> Props
map mapper (State list) =
    List.map (Tuple.mapFirst mapper) list



-- VIEW


type alias Effects msg =
    { onCloseButtonClick : ModelessId -> msg
    , onAnywhereClick : ModelessId -> msg
    , onDrag : ModelessId -> Int -> Int -> msg
    , onDragEnd : ModelessId -> msg
    , mediaViewerEffects : ModelessId -> MediaViewer.Effects msg
    }


{-| Payload must be resolved at runtime from State.
-}
type alias Props =
    List ( ResolvedPayload, Translate )


type ResolvedPayload
    = RawColumnItem ModelessId ColumnItem
    | MediaViewer ModelessId MediaViewer.Props


render : Effects msg -> Props -> Html msg
render eff props =
    let
        renderImpl index ( resolved, trans ) =
            case resolved of
                RawColumnItem id columnItem ->
                    withHeader index trans id "Source of Column Item" (RawColumnItem.render columnItem)

                MediaViewer id mProps ->
                    withHeader index trans id "Media Viewer" (MediaViewer.render (eff.mediaViewerEffects id) mProps)

        withHeader index trans id title content =
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
                    , if index == 0 then
                        Background.colorNote

                      else
                        Background.colorSub
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
    Html.Keyed.node "div" [ oneDark ] <|
        -- Reverse, since items at head must be shown at the end (stacked on the top)
        List.reverse (List.indexedMap renderImpl props)


styles : List Style
styles =
    [ s (c modelessWindowClass)
        [ ( "position", "fixed" )
        , ( "max-width", "90vw" )
        , ( "max-height", "90vh" )
        , ( "top", "10vh" )
        , ( "left", "10vw" )
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
