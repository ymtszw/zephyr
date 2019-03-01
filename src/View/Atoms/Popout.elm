module View.Atoms.Popout exposing
    ( State, Msg, init, update, sub
    , Config, Orientation, Control, Controls, withControl, node, withOne, withMany, anchoredVerticallyTo
    )

{-| Shows Html elements in "popout" manner.

"Popout" elements can literally pop out of their parent containers,
so that it can float above parent scrollbars and boundaries.
Useful for dropdowns, modals, and tooltips.


## Usage

    -- With single popout:
    Popout.withOne (myTooltip popoutState) <| \popoutControl ->
        myAnchor (Just popoutControl)

    -- Or with many:
    Popout.withMany [ myTooltip popoutState ] <| \popoutControls ->
        myAnchor (popoutControls "popoutElementId001")

    myAnchor controlMaybe =
        let
            handlers =
                case controlMaybe of
                    Just control ->
                        [ onMouseEnter control.show
                        , onMouseLeave control.hide
                        ]
                    Nothing ->
                        []
        in
        div
            ([ id "anchorElementId001"
            , style "width" "50px"
            , style "height" "30px"
            , style "border" "1px solid black"
            ] ++ handlers)
            [ text "Hover cursor on me to reveal a tooltip!" ]

    -- popoutState must be saved in your Model and supplied here
    myTooltip popoutState =
        Popout.control { id = "popoutElementId001" , msgTagger = PopoutMsg } <| \control ->
            Popout.node "div"
                { orientation = Popout.anchoredVerticallyTo "anchorElementId001"
                , state = popoutState
                }
                [ style "width" "40px"
                , style "height" "20px"
                , style "border" "1px solid red"
                ]
                [ text "I'm a tooltip!" ]

In this example `myAnchor` is placed inside the scope of `withOne` or `withMany`, since it has TWO purposes:

1.  as an anchor element for `myTooltip` and,
2.  as an element to which event handlers are attached

Notice that rendering function `withOne` or `withMany` creates a scope within your view
in which you can "control" visibility of your popout elements.

So, in fact, elements with the 2nd purpose MUST be placed inside the scope,
whereas one for the 1st purpose can actually live ANYWHERE in your view as long as it has a proper `id` attached.

@docs State, Msg, init, update, sub
@docs Config, Orientation, Control, Controls, withControl, node, withOne, withMany, anchoredVerticallyTo

-}

import AssocList as Dict exposing (Dict)
import Browser.Dom
import Html exposing (Attribute, Html, h1, text)
import Html.Attributes exposing (id, style)
import Id exposing (Id)
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
    -- Borrowing Phase as a phantom tag; just I'm being lazy
    Id String Phase


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
            ( State Dict.empty, Cmd.none )


queryAnchorElement : PopoutId -> AnchorId -> Cmd Msg
queryAnchorElement popoutId anchorId =
    Task.attempt (GotAnchorElement popoutId) (Browser.Dom.getElement (Id.to anchorId))



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
    }


withControl : Config msg -> State -> (Control msg -> Node msg) -> Popout msg
withControl config state toNode =
    let
        popoutId =
            Id.from config.id

        control =
            case config.orientation of
                AnchoredTo anchorId ->
                    { show = config.msgTagger (RequestShow popoutId anchorId)
                    , hide = config.msgTagger (RequestHide popoutId)
                    }
    in
    Popout ( popoutId, control, finalizeNode popoutId config.orientation state (toNode control) )


type alias Node msg =
    ( String, List (Attribute msg), List (Html msg) )


finalizeNode : PopoutId -> Orientation -> State -> Node msg -> Html msg
finalizeNode popoutId orientation (State dict) ( tagName, attrs, contents ) =
    case Dict.get popoutId dict of
        Just (Shown a) ->
            let
                positionedAttrs =
                    [ id (Id.to popoutId)
                    , style "position" "fixed"
                    , style "left" (String.fromFloat (a.element.x + a.element.width) ++ "px")
                    , style "top" (String.fromFloat a.element.y ++ "px")
                    ]
            in
            Html.node tagName (attrs ++ positionedAttrs) contents

        _ ->
            text ""


{-| Generate a popout element. Render using `withOne` or `withMany`.
-}
node : String -> List (Attribute msg) -> List (Html msg) -> Node msg
node tagName attrs contents =
    ( tagName, attrs, contents )


withOne : Popout msg -> (Control msg -> Html msg) -> List (Html msg)
withOne (Popout ( _, control, popout )) scopedContent =
    [ scopedContent control
    , popout
    ]


{-| Read-only dictionary of Controls.
-}
type alias Controls msg =
    String -> Maybe (Control msg)


withMany : List (Popout msg) -> (Controls msg -> Html msg) -> List (Html msg)
withMany popouts scopedContent =
    let
        ( popoutHtmls, controlDict ) =
            List.foldr reducer ( [], Dict.empty ) popouts

        reducer (Popout ( popoutId, control, popout )) ( accHtmls, accDict ) =
            ( popout :: accHtmls
            , Dict.insert popoutId control accDict
            )

        controls popoutIdStr =
            Dict.get (Id.from popoutIdStr) controlDict
    in
    scopedContent controls :: popoutHtmls


{-| Anchor a Popout element vertically to an anchor target element.
-}
anchoredVerticallyTo : String -> Orientation
anchoredVerticallyTo anchorIdStr =
    AnchoredVerticallyTo (Id.from anchorIdStr)
