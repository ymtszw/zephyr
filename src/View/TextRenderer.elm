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
import View.HtmlParts exposing (..)
import View.Parts exposing (columnCodeBlockMaxHeight, scale12, scaleByQuarter)


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
