module View exposing (body)

import Data.ColorTheme exposing (oneDark)
import Data.Model exposing (ColumnSwap, Model)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Html
import Html.Events
import Json.Decode as D exposing (Decoder)
import View.ColumnArea exposing (columnAreaEl)
import View.ConfigPane exposing (configPaneEl)
import View.Parts exposing (..)
import View.Sidebar exposing (sidebarEl)


body : Model -> List (Html.Html Msg)
body m =
    [ layout [ dragEventHandlers m.viewState.columnSwapMaybe ] (bodyEl m)
    , manualStyle
    ]


bodyEl : Model -> Element Msg
bodyEl model =
    backgroundEl <|
        row [ width fill, height fill, clipY ]
            [ sidebarEl model
            , configPaneEl model
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


dragEventHandlers : Maybe ColumnSwap -> Attribute Msg
dragEventHandlers columnSwapMaybe =
    case columnSwapMaybe of
        Just _ ->
            htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))

        Nothing ->
            noneAttr
