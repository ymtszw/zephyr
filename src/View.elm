module View exposing (body)

import Array exposing (Array)
import Data.Column exposing (Column)
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
import Json.Decode as D
import String exposing (fromInt)
import Url


body : Model -> List (Html.Html Msg)
body m =
    [ El.layout [] (bodyEl m)
    , fancyScroll
    ]


bodyEl : Model -> Element Msg
bodyEl model =
    El.row [ El.width El.fill, El.height El.fill ]
        [ sidebarEl model
        , columnsEl model
        ]



-- SIDEBAR


sidebarEl : Model -> Element Msg
sidebarEl { columns, env } =
    El.column
        [ El.width (El.px 50)
        , El.height (El.fill |> El.maximum env.clientHeight)
        , BG.color oneDarkBg
        ]
        [ El.el [ El.width El.fill, El.alignTop ] (columnButtonsEl columns)
        , El.el [ El.width El.fill, El.alignBottom ] otherButtonsEl
        ]


columnButtonsEl : Array Column -> Element Msg
columnButtonsEl columns =
    columns
        |> Array.indexedMap columnButtonEl
        |> Array.push ( "columnAddButton", columnAddButtonEl )
        |> Array.toList
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
columnsEl { columns, columnSwap, env } =
    backgroundEl <|
        Element.Keyed.row
            [ El.width El.fill
            , El.height (El.fill |> El.maximum env.clientHeight)
            , Font.regular
            ]
            (Array.indexedMap (columnKeyEl env.clientHeight columnSwap) columns |> Array.toList)


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


columnKeyEl : Int -> ColumnSwap -> Int -> Column -> ( String, Element Msg )
columnKeyEl clientHeight swap index { id, items } =
    ( "column_" ++ id
    , El.column
        (columnSwapAttrs swap index id <|
            [ El.width (El.fill |> El.minimum 320 |> El.maximum 860)
            , El.height (El.fill |> El.maximum clientHeight)
            , El.scrollbarY
            , BG.color oneDarkMain
            , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
            , BD.color oneDarkBg
            , Font.color oneDarkText
            ]
        )
        [ columnHeaderEl index id
        , items
            |> List.map itemEl
            |> El.column
                [ El.width El.fill
                , El.paddingXY 5 0
                ]
        ]
    )


columnSwapAttrs : ColumnSwap -> Int -> String -> List (El.Attribute Msg) -> List (El.Attribute Msg)
columnSwapAttrs { handleMaybe, hoverMaybe, swapping } index id otherAttrs =
    case handleMaybe of
        Just ( handleIndex, handleId ) ->
            if handleId == id then
                otherAttrs
                    ++ [ El.htmlAttribute (draggable "true")
                       , El.htmlAttribute (style "cursor" "grab")
                       , El.htmlAttribute (Html.Events.on "dragstart" (D.succeed DragStart))
                       , El.htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))
                       ]

            else if swapping then
                otherAttrs
                    ++ [ BD.width 5
                       , BD.rounded 10
                       , El.htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragHover id, True )))
                       , El.htmlAttribute (Html.Events.preventDefaultOn "dragover" (D.succeed ( DragHover id, True )))
                       , El.htmlAttribute (Html.Events.on "dragleave" (D.succeed DragLeave))
                       , El.htmlAttribute (Html.Events.on "drop" (D.succeed (Drop handleIndex index)))
                       ]
                    ++ (if hoverMaybe == Just id then
                            [ BD.color oneDarkSucc, BD.solid ]

                        else
                            [ BD.color oneDarkWarn, BD.dashed ]
                       )

            else
                otherAttrs

        Nothing ->
            otherAttrs


columnHeaderEl : Int -> String -> Element Msg
columnHeaderEl index id =
    El.el
        [ El.width El.fill
        , El.padding 10
        , BG.color oneDarkSub
        , Element.Events.onMouseEnter (MakeDraggable ( index, id ))
        , Element.Events.onMouseLeave GoUndraggable
        ]
        (El.text ("[PH] " ++ id))


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
