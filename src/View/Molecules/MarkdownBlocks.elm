module View.Molecules.MarkdownBlocks exposing (render)

import Html exposing (Html, blockquote, br, code, em, hr, img, li, ol, p, pre, span, strong, ul)
import Html.Attributes exposing (alt, src, start, style, title)
import Markdown.Block exposing (Block(..))
import Markdown.Inline exposing (Inline(..))
import TextParser
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (breakWords, nowrap)
import View.Atoms.Typography exposing (..)
import View.Style exposing (noAttr, none)


render : TextParser.ParseOptions -> String -> List (Html msg)
render opts raw =
    TextParser.map (renderBlock 0) (TextParser.parse opts raw)


renderBlock : Int -> Block a b -> Html msg
renderBlock quoteLevel block =
    case block of
        BlankLine _ ->
            none

        ThematicBreak ->
            hr [] []

        Heading _ level inlines ->
            let
                size =
                    if level <= 2 then
                        xProminent

                    else if level <= 4 then
                        prominent

                    else
                        noAttr
            in
            Html.node ("h" ++ String.fromInt level)
                [ breakWords
                , size
                , Border.bot1
                , Border.solid
                ]
                (List.map renderInline inlines)

        CodeBlock _ raw ->
            pre [ breakWords, padding2, Border.round2, Background.colorBg ] [ t raw ]

        Paragraph _ inlines ->
            p [ breakWords, padding2 ] (List.map renderInline inlines)

        BlockQuote blocks ->
            let
                bgColor =
                    if modBy 2 quoteLevel == 0 then
                        Background.colorSub

                    else
                        Background.colorMain
            in
            blockquote [ italic, breakWords, padding2, Border.gutter, bgColor ]
                (List.map (renderBlock (quoteLevel + 1)) blocks)

        List listBlock items ->
            let
                renderedListItems =
                    List.map (\blocks -> li [] (List.map (renderBlock quoteLevel) blocks)) items
            in
            case listBlock.type_ of
                Markdown.Block.Unordered ->
                    ul [ breakWords ] renderedListItems

                Markdown.Block.Ordered 1 ->
                    ol [ breakWords ] renderedListItems

                Markdown.Block.Ordered from ->
                    ol [ breakWords, start from ] renderedListItems

        PlainInlines inlines ->
            -- Found in list items; for easier implementation, we just wrap them in `span`
            span [] (List.map renderInline inlines)

        Markdown.Block.Custom _ _ ->
            none


renderInline : Inline a -> Html msg
renderInline inline =
    -- Note that images are inline by default. In order to make it appear as blocks,
    -- users should wrap them with blank lines (parsed as wrapper paragraph).
    case inline of
        Link urlStr titleMaybe inlines ->
            ntLink [ Maybe.withDefault noAttr (Maybe.map title titleMaybe) ]
                { url = urlStr, children = List.map renderInline inlines }

        _ ->
            Markdown.Inline.defaultHtml (Just renderInline) inline