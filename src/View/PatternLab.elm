module View.PatternLab exposing (main)

import Browser
import Html
import Html.Attributes
import View.Atom.Typography exposing (t)
import View.Stylesheet


main : Program () () ()
main =
    Browser.document
        { init = always ( (), Cmd.none )
        , view = always view
        , update = \() () -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        }


view : { title : String, body : List (Html.Html ()) }
view =
    { title = "Zephyr: Pattern Lab"
    , body =
        [ View.Stylesheet.render
        , atomsView
        ]
    }


atomsView : Html.Html ()
atomsView =
    Html.div []
        [ t "Pattern Lab is a catalogue of UI Atoms/Molecules in Zephyr app. "
        , t "This is a bare text, and it should be 12px in font-size. "
        ]
