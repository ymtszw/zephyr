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
        buffer =
            getBuffer editor

        baseAttrs =
            [ class textareaClass
            , flexItem
            , widthFill
            , onFocus (onToggleActive True)
            , onInput onInput_
            ]

        stateAttrs =
            if editorActive then
                let
                    bufferHeight =
                        if lines < 6 then
                            regularSize * 8

                        else
                            regularSize * 16

                    lines =
                        List.length (String.split "\n" buffer)
                in
                [ flexBasis (px bufferHeight), colorText, Background.colorNote ]

            else
                [ flexBasis (px (regularSize * 2)), colorNote, Background.colorSub ]
    in
    textarea (baseAttrs ++ stateAttrs) [ t buffer ]



-- STYLES


styles : List Style
styles =
    [ s (c textareaClass)
        [ ( "resize", "none" )
        , ( "transition", "all 0.15s" )
        ]
    ]


textareaClass : String
textareaClass =
    "editortextarea"
