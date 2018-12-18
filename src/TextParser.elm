module TextParser exposing (Parsed(..), ParseOptions, parse, defaultOptions)

{-| Parses various marked-up texts into intermediate representaitons (IR)
which can then be fed into TextRenderer for actual rendering.

Primarily it exists for parsing Markdown texts,
but it also parses other service-specific markups.

Using pure-Elm Markdown parser: <https://package.elm-lang.org/packages/pablohirafuji/elm-markdown/latest>

@docs Parsed, ParseOptions, parse, defaultOptions

-}

import Markdown.Block as Block exposing (Block(..))


{-| IR of parsed texts. Meant to be persisted for later uses.
-}
type Parsed
    = Parsed (List (Block () ()))


type alias ParseOptions =
    { markdown : Bool
    , autoLink : Bool
    }


parse : ParseOptions -> String -> Parsed
parse opts input =
    Parsed (Block.parse Nothing input)


defaultOptions : ParseOptions
defaultOptions =
    { markdown = True
    , autoLink = True
    }
