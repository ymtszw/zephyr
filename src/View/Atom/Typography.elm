module View.Atom.Typography exposing
    ( t
    , baseFontSize
    , baseFontSizeStyle
    )

{-| Typography Atoms.


## Html Renderers

@docs t


## Class Attributes

@docs baseFontSize


## Style Entries

@docs baseFontSizeStyle

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import View.Style exposing (..)



-- HTML


{-| Just emit a given String to Html. Synonym of Html.text
-}
t : String -> Html msg
t =
    Html.text



-- CLASS


{-| Class that explicitly enforce `baseFontSize`.
-}
baseFontSize : Attribute msg
baseFontSize =
    Attributes.class baseFontSizeClass



-- STYLE


{-| 12px.

This equals to the global default, so you do not need this in an element
where global default is not overridden.

-}
baseFontSizeStyle : Style
baseFontSizeStyle =
    c baseFontSizeClass [ ( "font-size", px (scale12 0) ) ]


baseFontSizeClass : String
baseFontSizeClass =
    "bfs"
