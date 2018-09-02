module Main exposing (main)

import Browser
import Html
import Json.Decode


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


init : Json.Decode.Value -> url -> key -> ( Model, Cmd Msg )
init _ _ _ =
    ( {}, Cmd.none )



-- MODEL


type alias Model =
    {}



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    { title = "Zephyr"
    , body = [ Html.text "Hi" ]
    }
