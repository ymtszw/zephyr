module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Data.Column as Column exposing (Column)
import Data.Types exposing (Model, Msg(..))
import Html
import Json.Decode as D
import Task
import View


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


init : D.Value -> url -> key -> ( Model, Cmd Msg )
init flags _ _ =
    ( Model
        (case D.decodeValue flagsDecoder flags of
            Ok ((_ :: _) as nonEmpty) ->
                nonEmpty

            _ ->
                List.repeat 1 Column.welcome
        )
        1024
    , adjustMaxHeight
    )


flagsDecoder : D.Decoder (List Column)
flagsDecoder =
    D.field "columns" (D.list Column.decoder)


adjustMaxHeight : Cmd Msg
adjustMaxHeight =
    Task.perform GetViewport getViewport



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize _ _ ->
            -- Not using onResize event values directly; they are basically innerWidth/Height which include scrollbars
            ( model, adjustMaxHeight )

        GetViewport { viewport } ->
            -- On the other hand, getViewport is using clientHeight, which does not include scrollbars
            ( { model | clientHeight = round viewport.height }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUB


sub : Model -> Sub Msg
sub _ =
    onResize Resize



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Zephyr"
    , body = View.body m
    }
