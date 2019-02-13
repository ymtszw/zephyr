module View.Organisms.Column.NewMessageEditor exposing (render)

import Data.ColumnEditor exposing (ColumnEditor, getBuffer)
import Html exposing (Html, div, textarea)
import Html.Events exposing (onInput)
import SelectArray exposing (SelectArray)
import View.Atoms.Border as Border
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)


type alias Effects msg =
    { onTextInput : String -> String -> msg
    }


type alias ColumnProps c =
    { c
        | id : String
        , editors : SelectArray ColumnEditor
        , editorActive : Bool
    }


render : Effects msg -> ColumnProps c -> Html msg
render eff column =
    let
        selectedEditor =
            SelectArray.selected column.editors
    in
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        , Border.colorBd
        , Border.bot1
        , Border.solid
        ]
        [ editorTextarea (eff.onTextInput column.id) column.editorActive selectedEditor
        ]


editorTextarea : (String -> msg) -> Bool -> ColumnEditor -> Html msg
editorTextarea onInput_ editorActive editor =
    textarea [ onInput onInput_ ] [ t (getBuffer editor) ]
