module View.Template.Main exposing
    ( Props, Effects, render
    , styles
    )

{-| Template of Main view.

It consumes necessary data as Props, which is effectively a ViewModel.

Also, you can inject event handlers as a set of functions (called Effects).
This allows you to wire different functions for any parts,
so that we can test our views in PatternLab! DI, anyone?

@docs Props, Effects, render
@docs styles

-}

import Html exposing (Html)
import View.Organism.Sidebar as Sidebar
import View.Style exposing (Style)


type alias Props =
    { sidebarProps : Sidebar.Props
    }


type alias Effects msg =
    { sidebarEffects : Sidebar.Effects msg
    }


render : Effects msg -> Props -> List (Html msg)
render eff p =
    [ Sidebar.render eff.sidebarEffects p.sidebarProps
    ]


styles : List Style
styles =
    []
