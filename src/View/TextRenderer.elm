module View.TextRenderer exposing (render)

{-| Render TextParser.Parsed into Elements.
-}

import Data.ColorTheme exposing (ColorTheme)
import Element exposing (Color, Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Json.Encode
import Markdown.Block as Block exposing (Block(..), ListBlock)
import Markdown.Inline as Inline exposing (Inline(..))
import Octicons
import TextParser exposing (Parsed(..))
import View.Parts exposing (columnCodeBlockMaxHeight, cssRgba, rectElementInnerPadding, rectElementRound, scale12, scaleByQuarter)


type alias RenderOptions =
    { theme : ColorTheme
    , fontSize : Int
    , maxMediaWidth : Int
    , parsed : Parsed
    }


render : RenderOptions -> List (Element msg)
render opts =
    let
        (Parsed blocks) =
            opts.parsed
    in
    renderImpl opts blocks []
        |> List.foldl (\h hs -> Element.html h :: hs) []


renderImpl : RenderOptions -> List (Block () ()) -> List (Html msg) -> List (Html msg)
renderImpl opts blocks acc =
    case blocks of
        [] ->
            acc

        (BlankLine _) :: xs ->
            renderImpl opts xs acc

        ThematicBreak :: xs ->
            renderImpl opts xs <| hr [] [] :: acc

        (Heading _ level inlines) :: xs ->
            -- Headings in feeds would interfere overall visual, so not enlarging
            let
                heading =
                    let
                        headingMarker =
                            [ span [ bold ] [ text (String.repeat level "#") ]
                            , text " "
                            ]

                        headingTexts =
                            List.concatMap (inlineToEls opts) inlines
                    in
                    paragraph [] (headingMarker ++ headingTexts)
            in
            renderImpl opts xs (heading :: acc)

        (CodeBlock codeOpts text) :: xs ->
            let
                codeBlock_ =
                    codeBlock [ fontSize (scaleByQuarter opts.fontSize -2) ]
                        { theme = opts.theme, maxHeight = columnCodeBlockMaxHeight, code = text }
            in
            renderImpl opts xs (codeBlock_ :: acc)

        (Paragraph _ []) :: xs ->
            renderImpl opts xs acc

        (Paragraph _ [ Text "" ]) :: xs ->
            renderImpl opts xs acc

        (Paragraph _ inlines) :: xs ->
            let
                para =
                    paragraph [] (List.concatMap (inlineToEls opts) inlines)
            in
            renderImpl opts xs (para :: acc)

        (BlockQuote quoted) :: xs ->
            column (gutteredConteinerAttrs opts.theme Nothing) (renderImpl opts quoted []) :: acc

        (List listOpts items) :: xs ->
            let
                listItems =
                    List.indexedMap (lazy4 listItemEl opts listOpts) items
            in
            renderImpl opts xs (List.reverse listItems ++ acc)

        (PlainInlines []) :: xs ->
            renderImpl opts xs acc

        (PlainInlines [ Text "" ]) :: xs ->
            renderImpl opts xs acc

        (PlainInlines inlines) :: xs ->
            let
                plainInlines =
                    List.concatMap (inlineToEls opts) inlines
            in
            renderImpl opts xs (List.reverse plainInlines ++ acc)

        (Block.Custom _ _) :: xs ->
            renderImpl opts xs acc


listItemEl : RenderOptions -> ListBlock -> Int -> List (Block () ()) -> Html msg
listItemEl opts listOpts index blocks =
    row [ widthFill, forceBreak ]
        [ lazy3 listMarker opts listOpts index
        , column [ alignTop ] (renderImpl opts blocks [])
        ]


listMarker : RenderOptions -> ListBlock -> Int -> Html msg
listMarker opts listOpts index =
    let
        listMarkerPaddingTop =
            (opts.fontSize - listMarkerSize) // 2
    in
    case listOpts.type_ of
        Block.Unordered ->
            octicon [ alignTop, paddingXY 0 listMarkerPaddingTop ]
                { size = listMarkerSize
                , color = opts.theme.text
                , shape =
                    case modBy 3 listOpts.indentLength of
                        2 ->
                            Octicons.primitiveDot

                        1 ->
                            Octicons.primitiveSquare

                        zero ->
                            Octicons.triangleRight
                }

        Block.Ordered originIndex ->
            let
                displayedIndex =
                    originIndex + index
            in
            div [ alignTop, paddingXY 0 listMarkerPaddingTop, style "user-select" "none" ] <|
                -- Use css content
                case modBy 3 listOpts.indentLength of
                    2 ->
                        [ text (String.fromInt displayedIndex ++ ">") ]

                    1 ->
                        [ text (String.fromInt displayedIndex ++ ")") ]

                    zero ->
                        [ text (String.fromInt displayedIndex ++ ".") ]


listMarkerSize : Int
listMarkerSize =
    scale12 -2


inlineToEls : RenderOptions -> Inline () -> List (Html msg)
inlineToEls opts inline =
    case inline of
        Text "" ->
            []

        Text s ->
            [ text s ]

        HardLineBreak ->
            [ br [] [] ]

        CodeInline c ->
            [ codeInline [] { theme = opts.theme, code = c } ]

        Link urlStr titleMaybe inlines ->
            [ newTabLink
                [ fontColor opts.theme.link
                , forceBreak
                , case titleMaybe of
                    Just t ->
                        title t

                    Nothing ->
                        noneAttr
                ]
                { url = urlStr
                , children = List.concatMap (inlineToEls opts) inlines
                }
            ]

        Image srcStr titleMaybe inlines ->
            -- XXX may need to force break?
            [ img
                [ src srcStr
                , alt <|
                    case titleMaybe of
                        Just title ->
                            title

                        Nothing ->
                            Inline.extractText inlines
                , maxWidth opts.maxMediaWidth
                ]
                []
            ]

        HtmlInline "code" _ inlines ->
            [ codeInline [] { theme = opts.theme, code = Inline.extractText inlines } ]

        HtmlInline tag attrs inlines ->
            -- For now we ignore other HtmlInline and just dump them as inline texts
            -- XXX Possibly support `<pre>` and lift them into CodeBlock?
            [ text (Inline.extractText inlines) ]

        Emphasis level inlines ->
            let
                decorated =
                    case level of
                        1 ->
                            span [ italic ]

                        2 ->
                            span [ bold ]

                        3 ->
                            span [ bold, underline ]

                        _ ->
                            span [ extraBold, underline ]
            in
            [ decorated <| List.concatMap (inlineToEls opts) inlines ]

        Inline.Custom () _ ->
            []



-- HTML Helpers


paragraph : List (Attribute msg) -> List (Html msg) -> Html msg
paragraph attrs children =
    let
        baseAttrs =
            [ forceBreak
            , style "line-height" "1.3em"
            , style "margin" "0"
            ]
    in
    p (baseAttrs ++ attrs) children


gutteredConteinerAttrs : ColorTheme -> Maybe Color -> List (Attribute msg)
gutteredConteinerAttrs theme gutterColor =
    [ widthFill
    , padding rectElementInnerPadding
    , style "border-left" (cssRgba (Maybe.withDefault theme.bd gutterColor) ++ " " ++ px gutterWidth)
    , style "border-radius" (String.join " " [ px gutterWidth, "0", "0", px gutterWidth ])
    ]


gutterWidth : Int
gutterWidth =
    3


codeBlock :
    List (Attribute msg)
    -> { theme : ColorTheme, maxHeight : Int, code : String }
    -> Html msg
codeBlock attrs opts =
    let
        baseAttrs =
            [ widthFill
            , maxHeight opts.maxHeight
            , padding rectElementInnerPadding
            , scrollbarY
            , bdRounded rectElementRound
            , bgColor opts.theme.sub
            , monospace
            , style "width" "100%"
            ]
    in
    pre (baseAttrs ++ attrs) [ text opts.code ]


maxHeight : Int -> Attribute msg
maxHeight mh =
    style "max-height" (px mh)


px : Int -> String
px i =
    String.fromInt i ++ "px"


padding : Int -> Attribute msg
padding pa =
    style "padding" (px pa)


scrollbarY : Attribute msg
scrollbarY =
    style "overflow-y" "auto"


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "flex-start"
        ]
        children


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "flex-start"
        ]
        children


widthFill : Attribute msg
widthFill =
    -- style "width" "100%"
    noneAttr


octicon : List (Attribute msg) -> { size : Int, color : Color, shape : Octicons.Options -> Html msg } -> Html msg
octicon attrs opts =
    div attrs
        [ Octicons.defaultOptions
            |> Octicons.color (cssRgba opts.color)
            |> Octicons.size opts.size
            |> opts.shape
        ]


alignTop : Attribute msg
alignTop =
    style "align-self" "flex-start"


noneAttr : Attribute msg
noneAttr =
    property "none" Json.Encode.null


newTabLink : List (Attribute msg) -> { url : String, children : List (Html msg) } -> Html msg
newTabLink attrs opts =
    let
        linkAttrs =
            [ href opts.url
            , rel "noopener noreferrer"
            , target "_blank"
            ]
    in
    a (linkAttrs ++ attrs) opts.children


maxWidth : Int -> Attribute msg
maxWidth mh =
    style "max-width" (px mh ++ "px")


italic : Attribute msg
italic =
    style "font-style" "italic"


bold : Attribute msg
bold =
    style "font-weight" "700"


extraBold : Attribute msg
extraBold =
    style "font-weight" "1000"


underline : Attribute msg
underline =
    style "text-decoration" "undeline"


codeInline : List (Attribute msg) -> { theme : ColorTheme, code : String } -> Html msg
codeInline attrs opts =
    let
        baseAttrs =
            [ paddingXY inlinePadding 0
            , bdRounded inlinePadding
            , bgColor opts.theme.text
            , fontColor opts.theme.err
            , monospace
            , forceBreak
            ]
    in
    span (baseAttrs ++ attrs) [ text opts.code ]


paddingXY : Int -> Int -> Attribute msg
paddingXY x y =
    style "padding" (String.join " " [ px y, px x, px y, px x ])


inlinePadding : Int
inlinePadding =
    2


bdRounded : Int -> Attribute msg
bdRounded r =
    style "border-radius" (px r)


fontColor : Color -> Attribute msg
fontColor color =
    style "color" (cssRgba color)


fontSize : Int -> Attribute msg
fontSize size =
    style "font-size" (px size)


bgColor : Color -> Attribute msg
bgColor color =
    style "background-color" (cssRgba color)


monospace : Attribute msg
monospace =
    style "font-family" "Lucida Console,Monaco,monospace"


forceBreak : Attribute msg
forceBreak =
    class breakClassName


breakClassName : String
breakClassName =
    "breakEl"
