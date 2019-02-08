module View.Organisms.Config.Pref exposing (Effects, Props, render, styles)

import Html exposing (Html, button, div, p)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Image as Image
import View.Atoms.Input as Input
import View.Atoms.Layout exposing (..)
import View.Atoms.Theme exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column as Column exposing (ColumnProps, Source(..))
import View.Molecules.Icon as Icon
import View.Molecules.Table as Table
import View.Style exposing (..)


type alias Effects msg =
    { onZephyrModeChange : Bool -> msg
    , onShowColumnButtonClick : String -> msg
    , onDeleteColumnButtonClick : String -> msg
    , onLoggingChange : Bool -> msg
    }


type alias Props =
    { zephyrMode : Bool
    , evictThreshold : Int
    , columnSlotsAvailable : Bool
    , shadowColumns : List ShadowColumn
    , logging : Bool
    }


type alias ShadowColumn =
    ColumnProps { id : String }


render : Effects msg -> Props -> Html msg
render eff props =
    div [ flexColumn, padding5, spacingColumn10 ]
        [ prefRow "Zephyr Mode" [ "When enabled, columns are automatically dismissed by LRU (least-recently-updated) manner." ] <|
            div [ flexColumn, spacingColumn5 ]
                [ Input.toggle [] { onChange = eff.onZephyrModeChange, checked = props.zephyrMode }
                , p [] [ t ("Max columns: " ++ String.fromInt props.evictThreshold) ]
                , desc
                    [ t "Automatically calculated based on your screen width. "
                    , t "If you pinned columns more than this limit, shadow columns do not automatically reappear."
                    ]
                ]
        , prefRow "Shadow Columns" [ "Columns currently aren't displayed. Automatically reappear when new messages arrived." ] <|
            shadowColumnsTable eff props.columnSlotsAvailable props.shadowColumns
        , prefRow "Logging" [ "Enables Elm events inspector at the bottom of this pane. This will SIGNIFICANTLY degrade application performance!" ] <|
            div [] [ Input.toggle [] { onChange = eff.onLoggingChange, checked = props.logging } ]
        ]


prefRow : String -> List String -> Html msg -> Html msg
prefRow title descriptions contents =
    div [ growRow, spacingRow5 ]
        [ div [ flexColumn, spacingColumn5 ]
            [ div [ xProminent ] [ t title ]
            , desc (List.map t descriptions)
            ]
        , contents
        ]


desc : List (Html msg) -> Html msg
desc texts =
    p [ colorNote ] texts


shadowColumnsTable : Effects msg -> Bool -> List ShadowColumn -> Html msg
shadowColumnsTable eff slotsAvailable shadowColumns =
    let
        columnCell sc =
            ( [ widthFill, theme sc ]
            , [ div [ flexRow, flexCenter, spacingRow5 ]
                    [ Column.icon20 sc
                    , div [ flexBasisAuto ] (Column.inlineTitle cellIconSize sc)
                    ]
              ]
            )

        cellIconSize =
            11

        actionCell sc =
            ( [ theme sc ]
            , [ div [ flexRow, flexCenter, spacingRow2 ]
                    [ showColumnButton (eff.onShowColumnButtonClick sc.id) slotsAvailable
                    , deleteColumnButton (eff.onDeleteColumnButtonClick sc.id)
                    ]
              ]
            )

        theme sc =
            case sc.sources of
                (SlackSource _) :: _ ->
                    aubergine

                _ ->
                    noAttr
    in
    Table.render []
        { columns = [ { header = "Column", cell = columnCell }, { header = "Action", cell = actionCell } ]
        , rowKey = .id
        , data = shadowColumns
        }


showColumnButton : msg -> Bool -> Html msg
showColumnButton onShowColumnButtonClick slotsAvailable =
    let
        showColumnButtonOcticonSize =
            14
    in
    button
        [ class showColumnButtonClass
        , flexItem
        , flexRow
        , flexCenter
        , padding2
        , Background.colorPrim
        , Image.fillText
        , disabled (not slotsAvailable)
        , onClick onShowColumnButtonClick
        ]
        [ Image.octicon { size = showColumnButtonOcticonSize, shape = Octicons.arrowRight }
        , t " Show"
        ]


deleteColumnButton : msg -> Html msg
deleteColumnButton onDeleteColumnButtonClick =
    Icon.octiconButton [ flexItem, Icon.rounded20, Image.fillErr, Background.transparent ]
        { onPress = onDeleteColumnButtonClick
        , size = shadowColumnIconSize
        , shape = Octicons.trashcan
        }



-- STYLES


styles : List Style
styles =
    [ s (c showColumnButtonClass)
        [ ( "width", px showColumnButtonWidth )
        , ( "height", px shadowColumnIconSize )
        , ( "justify-content", "center" )
        , ( "flex-basis", "auto" )
        ]
    ]


shadowColumnIconSize : Int
shadowColumnIconSize =
    -- Bigger than base font size
    20


showColumnButtonClass : String
showColumnButtonClass =
    "scshowbtn"


showColumnButtonWidth : Int
showColumnButtonWidth =
    70
