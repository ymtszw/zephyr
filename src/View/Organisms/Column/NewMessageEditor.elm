module View.Organisms.Column.NewMessageEditor exposing (render, styles)

import Data.ColumnEditor exposing (ColumnEditor, getBuffer)
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (class)
import Html.Events exposing (onFocus, onInput)
import SelectArray exposing (SelectArray)
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Style exposing (..)


type alias Effects msg =
    { onTextInput : String -> String -> msg
    , onToggleActive : String -> Bool -> msg
    }


type alias ColumnProps c =
    { c
        | id : String
        , editors : SelectArray ColumnEditor
        , editorActive : Bool
    }


render : Effects msg -> ColumnProps c -> Html msg
render eff c =
    let
        selectedEditor =
            SelectArray.selected c.editors
    in
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        , Border.colorBd
        , Border.bot1
        , Border.solid
        ]
        [ editorTextarea (eff.onTextInput c.id) (eff.onToggleActive c.id) c.editorActive selectedEditor
        ]


editorTextarea : (String -> msg) -> (Bool -> msg) -> Bool -> ColumnEditor -> Html msg
editorTextarea onInput_ onToggleActive editorActive editor =
    let
        baseAttrs =
            [ class textareaClass
            , onFocus (onToggleActive True)
            , onInput onInput_
            ]

        stateAttrs =
            if editorActive then
                [ colorText, Background.colorNote ]

            else
                [ colorNote, Background.colorSub ]
    in
    textarea (baseAttrs ++ stateAttrs) [ t (getBuffer editor) ]



-- STYLES


styles : List Style
styles =
    [ s (c textareaClass) [ ( "resize", "none" ) ]
    ]


textareaClass : String
textareaClass =
    "editortextarea"
