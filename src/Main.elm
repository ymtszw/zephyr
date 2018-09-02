module Main exposing (main)

import Browser
import Html
import Json.Decode as D


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


init : D.Value -> url -> key -> ( Model, Cmd Msg )
init v _ _ =
    let
        _ =
            Debug.log "decodedFlag" <|
                D.decodeValue (D.dict D.string) v
    in
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
