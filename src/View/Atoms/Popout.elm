module View.Atoms.Popout exposing
    ( Props, State
    , Orientation, node
    )

{-| Shows an Html element in "popout" manner.

"Popout" elements can literally pop out of their parent containers,
so that it can float above parent scrollbars and boundaries.
Useful for dropdowns, modals, and tooltips.


## Usage

    div
        [ id "anchorElementId001"
        , style "width" "50px"
        , style "height" "30px"
        , style "border" "1px solid black"
        , onMouseEnter (Popout.show "popoutElementId001")
        , onMouseLeave (Popout.hide "popoutElementId001")
        ]
        [ text "Hover cursor on me to reveal a tooltip!" ]

    -- Place this somewhere AFTER anchor element
    Popout.node "div"
        { id = "popoutElementId001"
        , orientation = Popout.anchoredTo "anchorElementId001"

        -- orientation = Popout.globalCenter
        , state = popoutState
        }
        [ style "width" "40px"
        , style "height" "20px"
        , style "border" "1px solid red"
        ]
        [ text "I'm a tooltip!" ]

@docs Props, State

-}

import AssocList as Dict exposing (Dict)
import Browser.Dom
import Html exposing (Attribute, Html)
import Id exposing (Id)


type alias Props =
    { id : String -- Required for retrieving bounding box of the anchor element
    , orientation : Orientation
    , state : State
    }


type State
    = State (Dict PopoutId Phase)


type alias PopoutId =
    Id String Phase



-- Borrowing Phase as a phantom tag; just I'm being lazy


type Phase
    = QueryingAnchorElement
    | Shown Browser.Dom.Element
    | Fading Browser.Dom.Element


type alias AnchorId =
    Id String Anchor


type Anchor
    = Anchor


type Orientation
    = TODO


{-| Generate a popout element.
-}
node : String -> Props -> List (Attribute msg) -> List (Html msg) -> Html msg
node tagName props attrs contents =
    -- TODO
    Html.node tagName attrs contents
