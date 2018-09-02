module View exposing (body)

import Data.Column exposing (Column)
import Data.Item exposing (Item, Media(..))
import Data.Types exposing (Model, Msg)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
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
bodyEl { columns, clientHeight } =
    El.row
        [ El.width El.fill
        , El.height (El.fill |> El.maximum clientHeight)
        , BG.color oneDarkBg
        ]
        (List.map (columnEl clientHeight) columns)


columnEl : Int -> Column -> Element Msg
columnEl clientHeight { items } =
    El.el
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
            El.image [ El.explain Debug.todo, El.width El.fill ]
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


oneDarkBd : El.Color
oneDarkBd =
    El.rgb255 62 65 71


oneDarkText : El.Color
oneDarkText =
    El.rgb255 220 221 222


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
