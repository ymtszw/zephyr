module TextParser exposing (Parsed(..), ParseOptions, parse, defaultOptions)

{-| Parses various marked-up texts into intermediate representaitons (IR)
which can then be fed into TextRenderer for actual rendering.

Primarily it exists for parsing Markdown texts,
but it also parses other service-specific markups.

Using pure-Elm Markdown parser: <https://package.elm-lang.org/packages/pablohirafuji/elm-markdown/latest>
Therefore it is slower than other Markdown parser solutions,
so you should consider parsing only once and storing `Parsed` IR for later uses.

@docs Parsed, ParseOptions, parse, defaultOptions

-}

import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))


{-| IR of parsed texts. Meant to be persisted for later uses.

Its internal data structure is Markdown.Block with removing
"original text" from items with children (in order to reduce IndexedDB usage).

-}
type Parsed
    = Parsed (List (Block () ()))


type alias ParseOptions =
    { markdown : Bool
    , autoLink : Bool
    , customInlineFormat : Maybe (Inline () -> Inline ())
    }


defaultOptions : ParseOptions
defaultOptions =
    { markdown = True
    , autoLink = True
    , customInlineFormat = Nothing
    }


parse : ParseOptions -> String -> Parsed
parse opts raw =
    if opts.markdown then
        Block.parse Nothing raw
            |> List.map (Block.walk (afterWark opts))
            |> Parsed

    else
        Parsed [ Paragraph "" [ Text raw ] ]


afterWark : ParseOptions -> Block () () -> Block () ()
afterWark opts block =
    removeOriginalText <|
        case opts.customInlineFormat of
            Just func ->
                Block.walkInlines func block

            Nothing ->
                block


removeOriginalText : Block () () -> Block () ()
removeOriginalText block =
    case block of
        Heading _ level inlines ->
            Heading "" level inlines

        Paragraph _ inlines ->
            Paragraph "" inlines

        _ ->
            block
