module View.TextRenderer exposing (render)

{-| Render TextParser.Parsed into Elements.
-}

import Data.ColorTheme exposing (ColorTheme)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Lazy exposing (..)
import Html
import Html.Attributes
import Markdown.Block as Block exposing (Block(..), ListBlock)
import Markdown.Inline as Inline exposing (Inline(..))
import Octicons
import TextParser exposing (Parsed(..))
import View.Parts exposing (..)


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


renderImpl : RenderOptions -> List (Block () ()) -> List (Element msg) -> List (Element msg)
renderImpl opts blocks acc =
    case blocks of
        [] ->
            List.reverse acc

        (BlankLine _) :: xs ->
            renderImpl opts xs acc

        ThematicBreak :: xs ->
            renderImpl opts xs <|
                el [ width fill, BD.widthXY 0 1, BD.color opts.theme.bd ] none
                    :: acc

        (Heading _ level inlines) :: xs ->
            -- Headings in feeds would interfere overall visual, so not enlarging
            let
                heading =
                    let
                        headingMarker =
                            [ el [ Font.bold ] (text (String.repeat level "#"))
                            , text " "
                            ]

                        headingTexts =
                            List.concatMap (inlineToEls opts) inlines
                    in
                    breakP [ width fill ] (headingMarker ++ headingTexts)
            in
            renderImpl opts xs (heading :: acc)

        (CodeBlock codeOpts text) :: xs ->
            let
                codeBlock_ =
                    codeBlock [ Font.size (scaleByQuarter opts.fontSize -2) ]
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
                    breakP [] (List.concatMap (inlineToEls opts) inlines)
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


listItemEl : RenderOptions -> ListBlock -> Int -> List (Block () ()) -> Element msg
listItemEl opts listOpts index blocks =
    row [ width fill, spacing spacingUnit, forceBreak ]
        [ lazy3 listMarker opts listOpts index
        , breakP [ width fill, alignTop ] (renderImpl opts blocks [])
        ]


listMarker : RenderOptions -> ListBlock -> Int -> Element msg
listMarker opts listOpts index =
    let
        listMarkerPaddingTop =
            (opts.fontSize - listMarkerSize) // 2
    in
    case listOpts.type_ of
        Block.Unordered ->
            octiconEl [ alignTop, paddingXY 0 listMarkerPaddingTop ]
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
            el [ alignTop, paddingXY 0 listMarkerPaddingTop, style "user-select" "none" ] <|
                case modBy 3 listOpts.indentLength of
                    2 ->
                        text (String.fromInt displayedIndex ++ ">")

                    1 ->
                        text (String.fromInt displayedIndex ++ ")")

                    zero ->
                        text (String.fromInt displayedIndex ++ ".")


listMarkerSize : Int
listMarkerSize =
    scale12 -2


inlineToEls : RenderOptions -> Inline () -> List (Element msg)
inlineToEls opts inline =
    case inline of
        Text "" ->
            []

        Text s ->
            [ breakT s ]

        HardLineBreak ->
            [ html (Html.br [] []) ]

        CodeInline c ->
            [ codeInline [] { theme = opts.theme, code = c } ]

        Link urlStr titleMaybe inlines ->
            let
                linkify i =
                    newTabLink
                        [ Font.color opts.theme.link
                        , style "display" "inline"
                        , forceBreak
                        , case titleMaybe of
                            Just title ->
                                htmlAttribute (Html.Attributes.title title)

                            Nothing ->
                                noneAttr
                        ]
                        { url = urlStr
                        , label = i
                        }
            in
            -- This is really ugly workaround; but elm-ui does not have inline <span>-equivalent
            List.map linkify <| List.concatMap (inlineToEls opts) inlines

        Image srcStr titleMaybe inlines ->
            -- XXX may need to force break?
            [ image [ width (shrink |> maximum opts.maxMediaWidth) ]
                { src = srcStr
                , description =
                    case titleMaybe of
                        Just title ->
                            title

                        Nothing ->
                            Inline.extractText inlines
                }
            ]

        HtmlInline "code" _ inlines ->
            [ codeInline [] { theme = opts.theme, code = Inline.extractText inlines } ]

        HtmlInline tag attrs inlines ->
            -- For now we ignore other HtmlInline and just dump them as inline texts
            -- XXX Possibly support `<pre>` and lift them into CodeBlock?
            [ text (Inline.extractText inlines) ]

        Emphasis level inlines ->
            if level < 2 then
                List.map (el [ forceBreak, Font.italic ]) <| List.concatMap (inlineToEls opts) inlines

            else
                List.map (el [ forceBreak, Font.bold ]) <| List.concatMap (inlineToEls opts) inlines

        Inline.Custom () _ ->
            []
