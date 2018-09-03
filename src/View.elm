module View exposing (body)

import Array exposing (Array)
import Data.Column exposing (Column)
import Data.Item exposing (Item, Media(..))
import Data.Types exposing (Model, Msg(..))
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Region exposing (description)
import Html
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


sidebarEl : Model -> Element Msg
sidebarEl { columns, clientHeight } =
    El.el
        [ El.width (El.px 50)
        , El.height (El.fill |> El.maximum clientHeight)
        , BG.color oneDarkBg
        ]
        (sidebarItemsEl columns)


sidebarItemsEl : Array Column -> Element Msg
sidebarItemsEl columns =
    columns
        |> Array.indexedMap sidebarItemEl
        |> Array.push ( "addColumnButton", addColumnButtonEl )
        |> Array.toList
        |> Element.Keyed.column [ El.width El.fill ]


sidebarItemEl : Int -> Column -> ( String, Element Msg )
sidebarItemEl index { id } =
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


addColumnButtonEl : Element Msg
addColumnButtonEl =
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


columnsEl : Model -> Element Msg
columnsEl { columns, clientHeight } =
    Element.Keyed.row
        [ El.width El.fill
        , El.height (El.fill |> El.maximum clientHeight)
        , BG.color oneDarkBg
        ]
        (Array.map (columnKeyEl clientHeight) columns |> Array.toList)


columnKeyEl : Int -> Column -> ( String, Element Msg )
columnKeyEl clientHeight { id, items } =
    ( "column_" ++ id
    , El.el
        [ El.width (El.fill |> El.minimum 320 |> El.maximum 860)
        , El.height (El.fill |> El.maximum clientHeight)
        , El.scrollbarY
        , El.paddingXY 5 0
        , BG.color oneDarkMain
        , BD.width 2
        , BD.color oneDarkBg
        , Font.color oneDarkText
        ]
        (items |> List.map itemEl |> El.column [ El.width El.fill ])
    )


itemEl : Item -> Element Msg
itemEl { message, mediaMaybe } =
    El.el [ El.width El.fill, El.paddingXY 10 15, BD.widthXY 0 1, BD.color oneDarkBd ] <|
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



-- FONT SIZE


scale16 : Int -> Int
scale16 =
    El.modular 16 1.25 >> round



-- UNSAFE STYLE


fancyScroll : Html.Html Msg
fancyScroll =
    Html.node "style" [] [ Html.text "::-webkit-scrollbar{display:none;}" ]
