module View.NewMessageEditor exposing (newMessageEditorEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.Msg exposing (Msg(..))
import Data.Producer exposing (ProducerRegistry)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Input
import View.Parts exposing (..)


newMessageEditorEl : ProducerRegistry -> Column.Column -> Element Msg
newMessageEditorEl pReg c =
    el
        [ width fill
        , height (px defaultEditorHeight)
        , BD.color frameColor
        , BD.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        ]
        none


defaultEditorHeight : Int
defaultEditorHeight =
    50


frameColor : Color
frameColor =
    oneDark.bd
