module View exposing (body)

import Array exposing (Array)
import Data.ColorTheme exposing (oneDark)
import Data.Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Core exposing (ColumnSwap, Model, Msg(..), UIState)
import Data.Item exposing (Item, Media(..), Metadata(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.TextRenderer exposing (TextRenderer)
import Dict
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Region exposing (description)
import Html
import Html.Attributes exposing (draggable, style)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Octicons
import String exposing (fromFloat)
import Url
import View.Parts exposing (octiconEl, scale12, squareIconEl)


body : Model -> List (Html.Html Msg)
body m =
    [ El.layout (dragEventHandlers m.uiState.columnSwapMaybe) (bodyEl m)
    , fancyScroll
    ]


bodyEl : Model -> Element Msg
bodyEl model =
    backgroundEl <|
        El.row [ El.width El.fill, El.height El.fill ]
            [ sidebarEl model
            , if model.uiState.configOpen then
                configPaneEl model

              else
                El.none
            , columnsEl model
            ]


backgroundEl : Element Msg -> Element Msg
backgroundEl contents =
    El.row
        [ BG.color oneDark.bg
        , El.width El.fill
        , El.height El.fill
        , El.inFront contents
        ]
        [ El.el
            [ El.centerY
            , El.centerX
            , Font.bold
            , Font.color oneDark.sub
            , Font.size (scale12 12)
            , Font.center
            , Font.family [ Font.serif ]
            ]
            (El.text "Zephyr")
        ]


dragEventHandlers : Maybe ColumnSwap -> List (El.Attribute Msg)
dragEventHandlers columnSwapMaybe =
    case columnSwapMaybe of
        Just _ ->
            [ El.htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))
            , El.htmlAttribute (Html.Events.preventDefaultOn "dragover" (D.succeed ( NoOp, True )))
            , El.htmlAttribute (Html.Events.preventDefaultOn "drop" (D.succeed ( NoOp, True )))
            ]

        Nothing ->
            []



-- SIDEBAR


sidebarEl : Model -> Element Msg
sidebarEl { columnStore, uiState, env } =
    El.column
        [ El.width (El.px 50)
        , El.height (El.fill |> El.maximum env.clientHeight)
        , El.paddingXY 0 10
        , BG.color oneDark.bg
        ]
        [ El.el [ El.width El.fill, El.alignTop ] (columnButtonsEl columnStore)
        , El.el [ El.width El.fill, El.alignBottom ] (otherButtonsEl uiState)
        ]


columnButtonsEl : ColumnStore -> Element Msg
columnButtonsEl columnStore =
    List.append (ColumnStore.indexedMap columnButtonEl columnStore) [ ( "columnAddButton", columnAddButtonEl ) ]
        |> Element.Keyed.column [ El.width El.fill, El.padding 5, El.spacingXY 0 10 ]


columnButtonEl : Int -> Column -> ( String, Element Msg )
columnButtonEl index { id } =
    ( "sidebarButton_" ++ id
    , El.el [ El.width El.fill ] <|
        Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 10
            , El.clip
            , Font.color oneDark.note
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just (DelColumn index), label = El.text "×" }
    )


columnAddButtonEl : Element Msg
columnAddButtonEl =
    El.el [ El.width El.fill ] <|
        Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 10
            , El.clip
            , Font.color oneDark.note
            , BD.dashed
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just AddColumn, label = El.text "+" }


otherButtonsEl : UIState -> Element Msg
otherButtonsEl uiState =
    El.column [ El.width El.fill, El.padding 5, El.spacingXY 0 10 ]
        [ Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 7
            , BD.rounded 10
            , if uiState.configOpen then
                BG.color oneDark.main

              else
                El.mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not uiState.configOpen))
            , label = octiconEl Octicons.gear
            }
        , El.link
            [ El.width El.fill
            , El.paddingXY 0 7
            , BD.rounded 10
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = octiconEl Octicons.markGithub
            }
        ]



-- COLUMNS


columnsEl : Model -> Element Msg
columnsEl { columnStore, uiState, env } =
    Element.Keyed.row
        [ El.width El.fill
        , El.height (El.fill |> El.maximum env.clientHeight)
        , Font.regular
        ]
        (ColumnStore.indexedMap (columnKeyEl env.clientHeight uiState) columnStore)


columnKeyEl : Int -> UIState -> Int -> Column -> ( String, Element Msg )
columnKeyEl clientHeight { columnSwappable, columnSwapMaybe } index column =
    ( "column_" ++ column.id
    , case columnSwapMaybe of
        Nothing ->
            notDraggedColumnEl clientHeight column <|
                if columnSwappable then
                    [ El.htmlAttribute (draggable "true")
                    , El.htmlAttribute (style "cursor" "all-scroll")
                    , El.htmlAttribute (Html.Events.on "dragstart" (onDragStart index column.id))
                    ]

                else
                    []

        Just swap ->
            if swap.grabbedId == column.id then
                draggedColumnEl clientHeight

            else
                notDraggedColumnEl clientHeight column <|
                    [ El.htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter index, True ))) ]
    )


notDraggedColumnEl : Int -> Column -> List (El.Attribute Msg) -> Element Msg
notDraggedColumnEl clientHeight column attrs =
    El.column
        (columnBaseAttrs clientHeight ++ attrs)
        [ columnHeaderEl column.id
        , column.items
            |> List.map itemEl
            |> El.column
                [ El.width El.fill
                , El.paddingXY 5 0
                , El.scrollbarY
                ]
        ]


columnBaseAttrs : Int -> List (El.Attribute Msg)
columnBaseAttrs clientHeight =
    [ El.width (El.fill |> El.minimum 320 |> El.maximum 860)
    , El.height (El.fill |> El.maximum clientHeight)
    , BG.color oneDark.main
    , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
    , BD.color oneDark.bg
    , Font.color oneDark.text
    ]


onDragStart : Int -> String -> Decoder Msg
onDragStart index id =
    let
        fireDependingOnDataTransfer types =
            case types of
                [] ->
                    -- If Column div elements are dragged, it should not have items attached in dataTransfer property
                    D.succeed (DragStart index id)

                _ ->
                    -- Otherwise something else (img, link, etc...) are dragged. Turn off swap mode
                    D.succeed (ToggleColumnSwappable False)
    in
    D.at [ "dataTransfer", "types" ] (D.list D.string)
        |> D.andThen fireDependingOnDataTransfer


columnHeaderEl : String -> Element Msg
columnHeaderEl id =
    El.el
        [ El.width El.fill
        , El.padding 10
        , BG.color oneDark.sub
        ]
        (El.text ("[PH] " ++ id))


draggedColumnEl : Int -> Element Msg
draggedColumnEl clientHeight =
    El.el (columnBaseAttrs clientHeight ++ [ BG.color oneDark.bg ]) El.none



-- ITEM


itemEl : Item -> Element Msg
itemEl item =
    El.row
        [ El.width El.fill
        , El.paddingXY 0 5
        , El.spacing 5
        , BD.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , BD.color oneDark.bd
        ]
        [ itemAvatarEl item
        , itemContentsEl item
        ]


itemAvatarEl : Item -> Element Msg
itemAvatarEl item =
    case item.metadata of
        DiscordMetadata dmd ->
            squareIconEl dmd.userName dmd.userAvatarUrlMaybe

        DefaultMetadata ->
            squareIconEl "Zephyr" Nothing


itemContentsEl : Item -> Element Msg
itemContentsEl item =
    case item.mediaMaybe of
        Just media ->
            El.textColumn [ El.spacingXY 0 10, El.width El.fill, El.alignTop ]
                [ messageToParagraph item.message
                , mediaEl media
                ]

        Nothing ->
            El.el [ El.width El.fill, El.alignTop ] (messageToParagraph item.message)


messageToParagraph : String -> Element Msg
messageToParagraph message =
    El.paragraph
        [ Font.size (scale12 2)
        , El.htmlAttribute (style "white-space" "pre-wrap")
        ]
        (Data.TextRenderer.default oneDark message)


mediaEl : Media -> Element Msg
mediaEl media =
    case media of
        Image url ->
            El.image [ El.width El.fill ]
                -- TODO pass description
                { src = Url.toString url, description = "Welcome image" }

        Movie _ ->
            -- Placeholder
            El.none



-- CONFIG PANE


configPaneEl : Model -> Element Msg
configPaneEl m =
    El.el
        [ El.width (El.fill |> El.minimum 480 |> El.maximum 860)
        , El.height (El.fill |> El.maximum m.env.clientHeight)
        , El.padding 15
        , El.scrollbarY
        , Font.color oneDark.text
        ]
        (configInnerEl m)


configInnerEl : Model -> Element Msg
configInnerEl m =
    El.column
        [ El.width El.fill
        , El.height El.fill
        ]
        [ El.map ProducerCtrl <| Producer.configsEl m.producerRegistry
        ]



-- UNSAFE STYLE


fancyScroll : Html.Html Msg
fancyScroll =
    Html.node "style" [] [ Html.text "::-webkit-scrollbar{display:none;}" ]
