module View.Organism.Config.Slack exposing (Props, render, styles)

import Html exposing (Html, div)
import View.Atom.Layout exposing (..)
import View.Style exposing (..)


type alias Props =
    {}


render : Props -> Html msg
render props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        ]
        []



-- STYLES


styles : List Style
styles =
    []
