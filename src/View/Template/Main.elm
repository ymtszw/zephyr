module View.Template.Main exposing
    ( Props, Effects, Contents, render
    , styles
    )

{-| Template of Main view.

It consumes necessary data as Props, which is effectively a ViewModel.

Also, you can inject event handlers as a set of functions (called Effects).
This allows you to wire different functions for any parts,
so that we can test our views in PatternLab! DI, anyone?

@docs Props, Effects, Contents, render
@docs styles

-}

import Html exposing (..)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout exposing (flexBasis, flexBasisAuto, flexColumn, flexGrow)
import View.Atom.Typography exposing (..)
import View.Molecule.Wallpaper as Wallpaper
import View.Organism.ColumnContainer as CCtnr
import View.Organism.Sidebar as Sidebar
import View.Style exposing (Style)


type alias Props c =
    { sidebarProps : Sidebar.Props
    , columnCtnrProps : CCtnr.Props c
    }


type alias Effects c msg =
    { sidebarEffects : Sidebar.Effects msg
    , columnCtnrEffects : CCtnr.Effects c msg
    }


type alias Contents c msg =
    { columnCtnrContents : CCtnr.Contents c msg
    }


render : Effects c msg -> Props c -> Contents c msg -> List (Html msg)
render eff p contents =
    [ Wallpaper.zephyr
    , Sidebar.render eff.sidebarEffects p.sidebarProps
    , CCtnr.render eff.columnCtnrEffects p.columnCtnrProps contents.columnCtnrContents
    ]


styles : List Style
styles =
    []
