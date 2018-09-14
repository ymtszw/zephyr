module View exposing (body)

import Array exposing (Array)
import Data.Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Core exposing (ColumnSwap, Model, Msg(..))
import Data.Item exposing (Item, Media(..))
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
import String exposing (fromInt)
import Url


body : Model -> List (Html.Html Msg)
body m =
    [ El.layout (dragEventHandlers m.columnSwapMaybe) (bodyEl m)
    , fancyScroll
    ]


bodyEl : Model -> Element Msg
bodyEl model =
    El.row [ El.width El.fill, El.height El.fill ]
        [ sidebarEl model
        , columnsEl model
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
sidebarEl { columnStore, env } =
    El.column
        [ El.width (El.px 50)
        , El.height (El.fill |> El.maximum env.clientHeight)
        , BG.color oneDarkBg
        ]
        [ El.el [ El.width El.fill, El.alignTop ] (columnButtonsEl columnStore)
        , El.el [ El.width El.fill, El.alignBottom ] otherButtonsEl
        ]


columnButtonsEl : ColumnStore -> Element Msg
columnButtonsEl columnStore =
    List.append (ColumnStore.indexedMap columnButtonEl columnStore) [ ( "columnAddButton", columnAddButtonEl ) ]
        |> Element.Keyed.column [ El.width El.fill ]


columnButtonEl : Int -> Column -> ( String, Element Msg )
columnButtonEl index { id } =
    ( "sidebarButton_" ++ id
    , El.el [ El.width El.fill, El.padding 5 ] <|
        Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 10
            , El.clip
            , Font.color oneDarkNote
            , BD.width 1
            , BD.color oneDarkNote
            , BD.rounded 10
            ]
            { onPress = Just (DelColumn index), label = El.text "×" }
    )


columnAddButtonEl : Element Msg
columnAddButtonEl =
    El.el [ El.width El.fill, El.padding 5 ] <|
        Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 10
            , El.clip
            , Font.color oneDarkNote
            , BD.dashed
            , BD.width 1
            , BD.color oneDarkNote
            , BD.rounded 10
            ]
            { onPress = Just AddColumn, label = El.text "+" }


otherButtonsEl : Element Msg
otherButtonsEl =
    El.column [ El.width El.fill, El.padding 5 ]
        [ El.link
            [ El.width El.fill
            , El.paddingXY 0 10
            , BG.color oneDarkSub
            , BD.rounded 10
            ]
            { url = "https://github.com/ymtszw/zephyr", label = El.text "</>" }
        ]



-- COLUMNS


columnsEl : Model -> Element Msg
columnsEl { columnStore, columnSwappable, columnSwapMaybe, env } =
    backgroundEl <|
        Element.Keyed.row
            [ El.width El.fill
            , El.height (El.fill |> El.maximum env.clientHeight)
            , Font.regular
            ]
            (ColumnStore.indexedMap (columnKeyEl env.clientHeight columnSwappable columnSwapMaybe) columnStore)


backgroundEl : Element Msg -> Element Msg
backgroundEl contents =
    El.row
        [ BG.color oneDarkBg
        , El.width El.fill
        , El.height El.fill
        , El.inFront contents
        ]
        [ El.el
            [ El.centerY
            , El.centerX
            , Font.bold
            , Font.color oneDarkSub
            , Font.size (scale16 12)
            , Font.center
            , Font.family [ Font.serif ]
            ]
            (El.text "Zephyr")
        ]


columnKeyEl : Int -> Bool -> Maybe ColumnSwap -> Int -> Column -> ( String, Element Msg )
columnKeyEl clientHeight swappable swapMaybe index column =
    ( "column_" ++ column.id
    , case swapMaybe of
        Nothing ->
            notDraggedColumnEl clientHeight column <|
                if swappable then
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
        ([ El.width (El.fill |> El.minimum 320 |> El.maximum 860)
         , El.height (El.fill |> El.maximum clientHeight)
         , El.scrollbarY
         , BG.color oneDarkMain
         , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
         , BD.color oneDarkBg
         , Font.color oneDarkText
         ]
            ++ attrs
        )
        [ columnHeaderEl column.id
        , column.items
            |> List.map itemEl
            |> El.column
                [ El.width El.fill
                , El.paddingXY 5 0
                ]
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
        , BG.color oneDarkSub
        ]
        (El.text ("[PH] " ++ id))


draggedColumnEl : Int -> Element Msg
draggedColumnEl clientHeight =
    El.el
        [ El.width (El.fill |> El.minimum 320 |> El.maximum 860)
        , El.height (El.fill |> El.maximum clientHeight)
        ]
        El.none



-- ITEM


itemEl : Item -> Element Msg
itemEl { message, mediaMaybe } =
    El.el [ El.width El.fill, El.paddingXY 10 15, BD.widthEach { top = 0, bottom = 2, left = 0, right = 0 }, BD.color oneDarkBd ] <|
        case mediaMaybe of
            Just media ->
                itemWithMedia message media

            Nothing ->
                itemTextOnly message


itemTextOnly : String -> Element Msg
itemTextOnly message =
    El.el [ El.width El.fill ] (messageToParagraph message)


messageToParagraph : String -> Element Msg
messageToParagraph message =
    El.paragraph [ Font.size (scale16 1) ] [ El.text message ]


itemWithMedia : String -> Media -> Element Msg
itemWithMedia message media =
    El.textColumn [ El.spacingXY 0 10, El.width El.fill ]
        [ messageToParagraph message
        , mediaEl media
        ]


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



-- COLOR


oneDarkBg : El.Color
oneDarkBg =
    El.rgb255 32 34 37


oneDarkMain : El.Color
oneDarkMain =
    El.rgb255 54 57 63


oneDarkSub : El.Color
oneDarkSub =
    El.rgb255 47 49 54


oneDarkBd : El.Color
oneDarkBd =
    El.rgb255 62 65 71


oneDarkText : El.Color
oneDarkText =
    El.rgb255 220 221 222


oneDarkNote : El.Color
oneDarkNote =
    El.rgb255 96 98 102


oneDarkLink : El.Color
oneDarkLink =
    El.rgb255 15 144 202


oneDarkSucc : El.Color
oneDarkSucc =
    El.rgb255 115 201 144


oneDarkWarn : El.Color
oneDarkWarn =
    El.rgb255 226 192 141


oneDarkErr : El.Color
oneDarkErr =
    El.rgb255 224 82 82



-- FONT SIZE


scale16 : Int -> Int
scale16 =
    El.modular 16 1.25 >> round



-- UNSAFE STYLE


fancyScroll : Html.Html Msg
fancyScroll =
    Html.node "style" [] [ Html.text "::-webkit-scrollbar{display:none;}" ]
