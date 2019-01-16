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
import View.Molecule.Wallpaper as Wallpaper
import View.Organism.ColumnContainer as CCtnr
import View.Organism.Sidebar as Sidebar
import View.Style exposing (Style)


type alias Props =
    { sidebarProps : Sidebar.Props
    , columnCtnrProps : CCtnr.Props ()
    }


type alias Effects msg =
    { sidebarEffects : Sidebar.Effects msg
    , columnCtnrEffects : CCtnr.Effects () msg
    }


render : Effects msg -> Props -> List (Html msg)
render eff p =
    [ Wallpaper.zephyr
    , Sidebar.render eff.sidebarEffects p.sidebarProps
    , CCtnr.render eff.columnCtnrEffects p.columnCtnrProps
    ]


styles : List Style
styles =
    []
