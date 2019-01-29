module View exposing (body)

import Data.ColorTheme exposing (oneDark)
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Html
import View.ColumnArea exposing (columnAreaEl)
import View.Parts exposing (..)
import View.Sidebar exposing (sidebarEl)


body : Model -> List (Html.Html Msg)
body m =
    [ manualStyle
    , layoutWith { options = [ focusStyle globalFocusStyle ] } [] (bodyEl m)
    ]


globalFocusStyle : FocusStyle
globalFocusStyle =
    -- Disabling default focus style, apply via manual CSS
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


bodyEl : Model -> Element Msg
bodyEl model =
    backgroundEl <|
        row [ width fill, height fill, clipY ]
            [ sidebarEl model
            , columnAreaEl model
            ]


backgroundEl : Element Msg -> Element Msg
backgroundEl contents =
    row
        [ BG.color oneDark.bg
        , width fill
        , height fill
        , inFront contents
        ]
        [ el
            [ centerY
            , centerX
            , Font.bold
            , Font.color oneDark.sub
            , Font.size (scale12 12)
            , Font.center
            , Font.family [ Font.serif ]
            ]
            (text "Zephyr")
        ]
