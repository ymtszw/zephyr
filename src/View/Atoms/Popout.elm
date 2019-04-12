module View.Atoms.Popout exposing
    ( State, Msg, init, hideUnsafe, update, sub, allClosed
    , Config, Orientation, Control, Popout, Node, generate, node, render, anchoredVerticallyTo
    )

{-| Shows Html elements in "popout" manner.

"Popout" elements can literally pop out of their parent containers,
so that it can float above parent scrollbars and boundaries.
Useful for dropdowns, modals, and tooltips.


## Usage

    Popout.render (myTooltip popoutState) <| \control ->
        Popout.node "div"
            [ style "overflow" "auto"
            , on "scroll" (succeed control.hide)
            ]
            [ div
                [ id "anchorElementId001"
                , onMouseEnter control.show
                , onMouseLeave control.hide
                ]
                [ text "Hover cursor on me to reveal a tooltip!" ]
            , div [] [ text lorem ]
            ]

    -- popoutState must be saved in your Model and supplied here
    myTooltip popoutState =
        let
            config =
                { id = "popoutElementId001"
                , msgTagger = PopoutMsg
                , orientation = Popout.anchoredVerticallyTo "anchorElementId001"
                }
        in
        Popout.generate config popoutState <| \control ->
            Popout.node "div" [] [ text "I'm a tooltip!" ]

In this example an anchor element is placed inside the scope of `render`, since it has TWO purposes:

1.  as an anchor element for `myTooltip` and,
2.  as an element to which event handlers are attached

Notice that `render` function creates a scope within your view
in which you can "control" visibility of your popout elements.

So, in fact, elements with the 2nd purpose MUST be placed inside the scope,
whereas one for the 1st purpose can actually live ANYWHERE in your view as long as it has a proper `id` attached.


## Scroll handling

You must be aware that when users scroll parent containers, anchor positions of popouts become off.
There are two approaches for this: (a) update anchor postions on scroll, or (b) close popouts on scroll.

Implementation for (b) is straighforward if the containers are placed inside `render` scopes.
Emit `hide` Msg provided from `Control` records on container scrolls, like so: `on "scroll" (succeed control.hide)`.

For (a), similarly, if we define `update` Msg, just emitting one should update anchor positions.
Although due to Task's asynchronous nature, Popouts follow anchors with slight delay.
Additionally, in order to tell anchor element is actually visible (within the viewport of scrolling container,)
we need to query more bounding box information.
Since it is cubmersome and too complex in implementation, we do not provide that. **Just hide on scroll**.

Both cases might not work well with keyboard navigation inside containers.

Regarding to global document scroll, you do not have to mind about that
since Popouts in this module are positioned with `absolute`.
Absolutely positioned elements are (ultimately) positioned relative to document origin (not to viewport origin)
thus Popouts should correctly move along with document scroll!

@docs State, Msg, init, hideUnsafe, update, sub, allClosed
@docs Config, Orientation, Control, Popout, Node, generate, node, render, anchoredVerticallyTo

-}

import AssocList as Dict exposing (Dict)
import Browser.Dom
import Browser.Events
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (id, style)
import Id exposing (Id)
import Json.Decode exposing (Decoder, fail, field, lazy, string, succeed)
import Json.DecodeExtra exposing (do, when)
import Task


{-| Dictionary of popout states. This is what you store in your Model.

Popout nodes are stored in id-based dictionary.
Popout nodes must be specified by ids in order to activate them.
When a popout node is activated, a new state entry is inserted to the dictionary.
When it is deactivated, the entry is removed.

So, an application can have any number of popout nodes in it.
It is possible to open some (or even, all!) of them at once.

-}
type State
    = State (Dict PopoutId Phase)


type alias PopoutId =
    Id String (Popout Msg)


{-| Phase of each popout node.

Currently only supports anchored variants.
TODO Diverge anchored variants and gloabally-positioned variants.

-}
type Phase
    = QueryingAnchorElement
    | Shown Browser.Dom.Element



-- EFFECTS


init : State
init =
    State Dict.empty


type Msg
    = RequestShow PopoutId AnchorId
    | GotAnchorElement PopoutId (Result Browser.Dom.Error Browser.Dom.Element)
    | RequestHide PopoutId
    | HideAll


hideUnsafe : String -> Msg
hideUnsafe idStr =
    RequestHide (Id.from idStr)


update : Msg -> State -> ( State, Cmd Msg )
update msg (State dict) =
    case msg of
        RequestShow popoutId anchorId ->
            case Dict.get popoutId dict of
                Just QueryingAnchorElement ->
                    -- Not dispatching duplicated queryAnchorElement
                    ( State dict, Cmd.none )

                Just (Shown _) ->
                    -- Keep current phase, but dispatch new queryAnchorElement
                    ( State dict, queryAnchorElement popoutId anchorId )

                Nothing ->
                    ( State (Dict.insert popoutId QueryingAnchorElement dict)
                    , queryAnchorElement popoutId anchorId
                    )

        GotAnchorElement popoutId (Ok anchorElement) ->
            let
                updater =
                    Maybe.map (\_ -> Shown anchorElement)
            in
            ( State (Dict.update popoutId updater dict), Cmd.none )

        GotAnchorElement popoutId (Err (Browser.Dom.NotFound _)) ->
            ( State (Dict.remove popoutId dict), Cmd.none )

        RequestHide popoutId ->
            ( State (Dict.remove popoutId dict), Cmd.none )

        HideAll ->
            let
                keepCurrentlyQueried _ phase =
                    case phase of
                        QueryingAnchorElement ->
                            True

                        Shown _ ->
                            False
            in
            ( State (Dict.filter keepCurrentlyQueried dict), Cmd.none )


queryAnchorElement : PopoutId -> AnchorId -> Cmd Msg
queryAnchorElement popoutId anchorId =
    Task.attempt (GotAnchorElement popoutId) (Browser.Dom.getElement (Id.to anchorId))


sub : State -> Sub Msg
sub (State dict) =
    if Dict.isEmpty dict then
        Sub.none

    else
        Sub.batch
            [ Browser.Events.onResize (\_ _ -> HideAll)
            , Browser.Events.onKeyDown <|
                when (field "key" string) ((==) "Escape") (succeed HideAll)
            , Browser.Events.onClick <|
                -- We do this because there isn't widely implemented property yet for getting Event bubbling path.
                -- Event.path is Chrome-only, Event.composedPath() is a function.
                field "target" recursivelyCheckIfClickout
            ]


recursivelyCheckIfClickout : Decoder Msg
recursivelyCheckIfClickout =
    do (field "tagName" string) <|
        \tagName ->
            case tagName of
                "HTML" ->
                    -- Reached Element Node root
                    succeed HideAll

                _ ->
                    do (field "id" string) <|
                        \id ->
                            if String.startsWith popoutIdPrefix id then
                                fail "Click inside Popout node"

                            else
                                field "parentElement" (lazy (\_ -> recursivelyCheckIfClickout))


allClosed : State -> Bool
allClosed (State dict) =
    Dict.isEmpty dict



-- VIEW


{-| Static configuration of your Popout node.

This record must exist in your view functions and not be stored in your Model.

-}
type alias Config msg =
    { id : String
    , msgTagger : Msg -> msg
    , orientation : Orientation
    }


type Orientation
    = AnchoredVerticallyTo AnchorId


type alias AnchorId =
    Id String Anchor


type Anchor
    = Anchor


type Popout msg
    = Popout ( PopoutId, Control msg, Html msg )


{-| "Controller" or a Popout element.

This record is a source of messages that can be dispatched from your view
in order to show or hide the Popout element.

-}
type alias Control msg =
    { show : msg
    , hide : msg
    , toggle : msg
    }


type Node msg
    = Node String (List (Attribute msg)) (List (Html msg))


{-| Generate a Popout element. Use `node` to construct outermost node.
-}
generate : Config msg -> State -> (Control msg -> Node msg) -> Popout msg
generate config (State dict) toNode =
    let
        popoutId =
            Id.from config.id

        control =
            case config.orientation of
                AnchoredVerticallyTo anchorId ->
                    { show = config.msgTagger (RequestShow popoutId anchorId)
                    , hide = config.msgTagger (RequestHide popoutId)
                    , toggle =
                        case Dict.get popoutId dict of
                            Just (Shown _) ->
                                config.msgTagger (RequestHide popoutId)

                            _ ->
                                config.msgTagger (RequestShow popoutId anchorId)
                    }

        finalizedNode =
            case Dict.get popoutId dict of
                Just (Shown a) ->
                    let
                        (Node tagName attrs contents) =
                            toNode control

                        positionedAttrs =
                            id (popoutIdPrefix ++ Id.to popoutId)
                                :: style "position" "absolute"
                                :: calculatePosition config.orientation a
                    in
                    Html.node tagName (attrs ++ positionedAttrs) contents

                _ ->
                    text ""
    in
    Popout ( popoutId, control, finalizedNode )


popoutIdPrefix : String
popoutIdPrefix =
    "popout__"


calculatePosition : Orientation -> Browser.Dom.Element -> List (Attribute msg)
calculatePosition orientation a =
    case orientation of
        AnchoredVerticallyTo _ ->
            let
                marginAboveAnchorInViewport =
                    max 0 (anchorTop - viewportTop)

                marginBelowAnchorInViewport =
                    max 0 (viewportBottom - anchorBottom)

                anchorTop =
                    a.element.y

                anchorBottom =
                    a.element.y + a.element.height

                viewportTop =
                    a.viewport.y

                viewportBottom =
                    a.viewport.y + a.viewport.height
            in
            if marginBelowAnchorInViewport >= marginAboveAnchorInViewport then
                []

            else
                [ style "transform" ("translateY(calc(-" ++ String.fromFloat a.element.height ++ "px - 100%))") ]


{-| Creates a node related to your Popout. Use with `generate` or `render`.
-}
node : String -> List (Attribute msg) -> List (Html msg) -> Node msg
node tagName attrs contents =
    Node tagName attrs contents


{-| Render HTML of your Popout and Container.
-}
render : Popout msg -> (Control msg -> Node msg) -> Html msg
render (Popout ( _, control, popout )) innerContent =
    let
        (Node tagName attrs contents) =
            innerContent control
    in
    Html.node tagName attrs (contents ++ [ popout ])


{-| Anchor a Popout element vertically to an anchor target element.
-}
anchoredVerticallyTo : String -> Orientation
anchoredVerticallyTo anchorIdStr =
    AnchoredVerticallyTo (Id.from anchorIdStr)
