module View.Molecules.MarkdownBlocks exposing (render, sampleSource)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown.Block exposing (Block(..))
import Markdown.Inline exposing (Inline(..))
import TextParser
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (breakWords)
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
            -- Not enlarging fonts, since markdowns in user contents can oftentimes be ill-formatted,
            -- such that subsequent paragraphs are incorrectly included in headings, breaking styles.
            Html.node ("h" ++ String.fromInt level)
                [ breakWords
                , Border.bot1
                , Border.solid
                ]
                (List.map renderInline inlines)

        CodeBlock _ raw ->
            pre [ minuscule, breakWords, padding2, Border.round2, Background.colorBg ] [ t raw ]

        Paragraph _ inlines ->
            p [ breakWords ] (List.map renderInline inlines)

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


sampleSource : String
sampleSource =
    """# Title h1
*Lorem* ipsum **dolor** sit ***amet***, consectetur ****adipisicing**** elit, sed do eiusmod tempor
_incididunt_ ut __labore__ et ___dolore___ magna ____aliqua____. Ut enim ad minim veniam, quis nostrud
exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute
irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui
officia deserunt mollit anim id est laborum.

Horizontal rule below:

---
## Title h2
いろはにほへと散りぬるをわかよ誰そ常ならむ有為の奥山今日越えてあさきゆめみしゑひもせすん

- This is an unordered list
- Can be nested
    - like
        - this
- What happens if there are very long lines and words?
  abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123
    - More nest
        - And more
            - And yet more
                - This is the end

0. And this is an ordered list, starting from zero
1. Should be nestable
    1. Like
    2. this
        1. More nest
            1. And more
                1. And yet more
                    1. This is the end

### Title h3
With code: `<code>With Code</code>`,\\
and hard line breaks (`\\n`)!\\
Raw link: <https://example.com>, and [Link with title][link]

[link]: https://example.com "link"

#### Title h4
```
This is a fenced code block.
Should respect line breaks!
```

    This is an indented code block.
        Should respect line breaks and further indents.
    This is a very long line in a code block. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    And a very long word abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123abcd0123

##### Title h5
> This is a block quote.
>
> Within it, everything is baseline italic.
>
> > Can be nested!!
> >
> > > Nested and even colored!!
>
> ```
> Can have code block!
> ```
>
> - Can have lists!
>     - Like
>     - This
> - Foo!

###### Title h6

Images are inline elements. ![20x20 image](https://picsum.photos/20/20) Like this.

And they are not contained by default.
You can however apply styles to `<img>` under the container.
Utilize `object-fit`!

[![300x500 image](https://picsum.photos/300/500)](https://example.com)

They can be linked, of course.
"""
