module Main exposing (main)

import Array
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Data.Array as Array
import Data.Column as Column exposing (Column)
import Data.Types exposing (Model, Msg(..))
import Data.UniqueId as UniqueId
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
    let
        ( columns, idGen ) =
            initColumns UniqueId.init flags
    in
    ( Model (Array.fromList columns) clientHeightFallback idGen
    , adjustMaxHeight
    )


initColumns : UniqueId.Generator -> D.Value -> ( List Column, UniqueId.Generator )
initColumns idGen flags =
    case D.decodeValue flagsDecoder flags of
        Ok ((_ :: _) as nonEmpty) ->
            List.foldr
                (\fromFlag ( accColumns, accIdGen ) ->
                    let
                        ( newId, newIdGen ) =
                            UniqueId.gen "column" accIdGen
                    in
                    ( { fromFlag | id = newId } :: accColumns, newIdGen )
                )
                ( [], idGen )
                nonEmpty

        _ ->
            List.foldr
                (\welcomeFun ( accColumns, accIdGen ) ->
                    let
                        ( newId, newIdGen ) =
                            UniqueId.gen "column" accIdGen
                    in
                    ( welcomeFun newId :: accColumns, newIdGen )
                )
                ( [], idGen )
                (List.repeat 4 Column.welcome)


clientHeightFallback : Int
clientHeightFallback =
    1024


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

        AddColumn ->
            let
                ( newId, newIdGen ) =
                    UniqueId.gen "column" model.idGen
            in
            ( { model | columns = Array.push (Column.welcome newId) model.columns, idGen = newIdGen }, Cmd.none )

        DelColumn index ->
            ( { model | columns = Array.removeAt index model.columns }, Cmd.none )

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
