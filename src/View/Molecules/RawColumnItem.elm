module View.Molecules.RawColumnItem exposing (render)

import Data.Column as Column exposing (ColumnItem)
import Html exposing (Html, pre, text)
import Json.Encode
import View.Atoms.Background as Background
import View.Atoms.Border as Border


render : ColumnItem -> Html msg
render ci =
    pre [ Background.colorBg, Border.round5 ]
        [ text (Json.Encode.encode 2 (Column.encodeColumnItem ci)) ]
