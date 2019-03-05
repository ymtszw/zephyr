module View.Molecules.MarkdownBlocks exposing (render)

import Html exposing (Html)
import Markdown.Block exposing (Block(..))
import TextParser
import View.Style exposing (none)


render : TextParser.ParseOptions -> String -> List (Html msg)
render opts raw =
    TextParser.map renderBlock (TextParser.parse opts raw)


renderBlock : Block () () -> Html msg
renderBlock block =
    case block of
        BlankLine stringString ->
            none

        ThematicBreak ->
            none

        Heading stringString intBasics iInlineInlineMarkdownListList ->
            none

        CodeBlock codeBlockBlockMarkdown stringString ->
            none

        Paragraph stringString iInlineInlineMarkdownListList ->
            none

        BlockQuote ibBlockBlockMarkdownListList ->
            none

        List listBlockBlockMarkdown ibBlockBlockMarkdownListListListList ->
            none

        PlainInlines iInlineInlineMarkdownListList ->
            none

        Custom () ibBlockBlockMarkdownListList ->
            none
