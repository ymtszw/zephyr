module View.Molecules.MarkdownBlocks exposing (render)

import Html exposing (Html)
import TextParser exposing (Parsed)


render : TextParser.ParseOptions -> String -> List (Html msg)
render opts raw =
    []
