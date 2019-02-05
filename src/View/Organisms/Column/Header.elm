module View.Organisms.Column.Header exposing (Effects, Source(..), render, styles)

import Html exposing (Attribute, Html, button, div, span)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (succeed)
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (forceBreak)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { onDragstart : Bool -> Int -> String -> msg
    , onHeaderClick : Maybe msg
    , onPinButtonClick : String -> Bool -> msg
    , onConfigToggleButtonClick : String -> Bool -> msg
    , onDismissButtonClick : Int -> msg
    }


type Source
    = DiscordSource { channelName : String, guildIcon : Maybe String }
    | SlackSource { convName : String, teamIcon : Maybe String, isPrivate : Bool }


render :
    Effects msg
    -> Int
    ->
        { c
            | id : String
            , sources : List Source
            , filters : List String
            , pinned : Bool
            , configOpen : Bool
        }
    -> Html msg
render eff index column =
    div
        [ flexRow
        , flexCenter
        , flexBasisAuto
        , padding5
        , spacingRow5
        , Background.colorSub
        ]
        [ grabbableIcon (eff.onDragstart column.pinned index column.id) column
        , headerText eff.onHeaderClick column.sources column.filters
        , if column.pinned then
            none

          else
            headerButton [ Image.hovSucc ] (eff.onDismissButtonClick index) Octicons.check
        , let
            innerAttrs =
                -- Rotate inner contents, not the button itself, to keep the clickable area stable
                if column.pinned then
                    [ class pinButtonClass, Image.fillWarn, Image.rotate45 ]

                else
                    [ class pinButtonClass ]
          in
          button
            [ flexItem
            , flexBasisAuto
            , noPadding
            , Image.hovWarn
            , Border.round2
            , Background.transparent
            , Background.hovBd
            , onClick (eff.onPinButtonClick column.id (not column.pinned))
            ]
            [ div innerAttrs [ Image.octicon { size = octiconSize, shape = Octicons.pin } ] ]
        , headerButton
            [ if column.configOpen then
                Image.fillText

              else
                noAttr
            , Image.hovText
            ]
            (eff.onConfigToggleButtonClick column.id (not column.configOpen))
            Octicons.settings
        ]


headerButton : List (Attribute msg) -> msg -> (Octicons.Options -> Html msg) -> Html msg
headerButton attrs onPress shape =
    let
        baseAttrs =
            [ flexItem
            , flexBasisAuto
            , noPadding
            , Border.round2
            , Background.transparent
            , Background.hovBd
            ]
    in
    Icon.octiconButton (baseAttrs ++ attrs)
        { onPress = onPress
        , size = octiconSize
        , shape = shape
        }


octiconSize : Int
octiconSize =
    30


grabbableIcon : msg -> { c | id : String, sources : List Source } -> Html msg
grabbableIcon onDragstart column =
    div
        [ flexBasisAuto
        , draggable "true"
        , on "dragstart" (succeed onDragstart)
        , Cursor.allScroll
        ]
        [ sourceIcon column.sources
        ]


sourceIcon : List Source -> Html msg
sourceIcon sources =
    case sources of
        [] ->
            Icon.abbr [ Icon.rounded30, serif, sizeTitle ] "Zephyr"

        s :: _ ->
            let
                ( bottomRight, content ) =
                    case s of
                        DiscordSource opts ->
                            ( Icon.discordBadge14
                            , Icon.imgOrAbbr [ Icon.rounded30, serif, sizeTitle ] opts.channelName opts.guildIcon
                            )

                        SlackSource opts ->
                            ( Icon.slackBadge14
                            , Icon.imgOrAbbr [ Icon.rounded30, serif, sizeTitle ] opts.convName opts.teamIcon
                            )
            in
            withBadge [ badgeOutset ]
                { topRight = Nothing
                , bottomRight = Just bottomRight
                , content = content
                }


headerText : Maybe msg -> List Source -> List String -> Html msg
headerText onHeaderClick sources filters =
    let
        baseAttrs =
            [ flexGrow, flexColumn, spacingColumn2, forceBreak ]

        headerClicerAttrs =
            case onHeaderClick of
                Just onPress ->
                    [ onClick onPress
                    , Cursor.pointer
                    ]

                Nothing ->
                    []

        mainText =
            div [ bold, sizeHeadline ]

        sourcesToMain =
            List.map sourceText >> List.intersperse [ t ", " ] >> List.concat
    in
    div (baseAttrs ++ headerClicerAttrs) <|
        case ( sources, filters ) of
            ( [], [] ) ->
                [ mainText [ t "New Column" ] ]

            ( _, [] ) ->
                [ mainText (sourcesToMain sources) ]

            ( [], _ ) ->
                [ mainText [ t (String.join ", " filters) ] ]

            ( _, _ ) ->
                [ mainText (sourcesToMain sources)
                , div [ colorNote ] [ t (String.join ", " filters) ]
                ]


sourceText : Source -> List (Html msg)
sourceText source =
    case source of
        DiscordSource { channelName } ->
            [ t ("#" ++ channelName) ]

        SlackSource { convName, isPrivate } ->
            [ if isPrivate then
                span [ Image.fillText ] [ Image.octicon { size = headlineSize, shape = Octicons.lock } ]

              else
                t "#"
            , t convName
            ]


headlineSize : Int
headlineSize =
    15


styles : List Style
styles =
    [ s (c pinButtonClass) [ ( "transition", "transform 0.2s" ) ] ]


pinButtonClass : String
pinButtonClass =
    "pinbtn"