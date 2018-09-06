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
import Json.Encode as E
import Ports
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
        Ok ((_ :: _) as nonEmptyColumns) ->
            let
                applyId fromFlag ( accColumns, accIdGen ) =
                    UniqueId.genAndMap "column" accIdGen <|
                        \newId ->
                            { fromFlag | id = newId } :: accColumns
            in
            List.foldr applyId ( [], idGen ) nonEmptyColumns

        _ ->
            UniqueId.genAndMap "column" idGen <| \newId -> [ Column.welcome newId ]


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
            persist ( { model | columns = Array.push (Column.welcome newId) model.columns, idGen = newIdGen }, Cmd.none )

        DelColumn index ->
            persist ( { model | columns = Array.removeAt index model.columns }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


persist : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
persist ( model, cmd ) =
    ( model, Cmd.batch [ cmd, Ports.sendToJs (encodeToFlags model) ] )


encodeToFlags : Model -> E.Value
encodeToFlags { columns } =
    E.object
        [ ( "columns", E.array Column.encoder columns )
        ]



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
