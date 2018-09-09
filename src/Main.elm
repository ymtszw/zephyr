module Main exposing (main)

import Array
import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav exposing (Key)
import Data.Array as Array
import Data.Column as Column exposing (Column)
import Data.Core exposing (Env, Model, Msg(..), initModel, welcomeModel)
import Data.UniqueId as UniqueId
import Html
import Json.Decode as D
import Json.Encode as E
import Ports
import Task
import Url
import View



-- INIT


init : Env -> url -> Key -> ( Model, Cmd Msg )
init env _ navKey =
    ( initModel env navKey, adjustMaxHeight )


adjustMaxHeight : Cmd Msg
adjustMaxHeight =
    Task.perform GetViewport getViewport



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ env } as model) =
    case msg of
        Resize _ _ ->
            -- Not using onResize event values directly; they are basically innerWidth/Height which include scrollbars
            ( model, adjustMaxHeight )

        GetViewport { viewport } ->
            -- On the other hand, getViewport is using clientHeight, which does not include scrollbars
            ( { model | env = { env | clientHeight = round viewport.height } }, Cmd.none )

        LinkClicked (Internal url) ->
            ( model, Nav.pushUrl model.navKey (Url.toString url) )

        LinkClicked (External url) ->
            ( model, Nav.load url )

        AddColumn ->
            let
                ( newId, newIdGen ) =
                    UniqueId.gen "column" model.idGen
            in
            persist ( { model | columns = Array.push (Column.welcome newId) model.columns, idGen = newIdGen }, Cmd.none )

        DelColumn index ->
            persist ( { model | columns = Array.removeAt index model.columns }, Cmd.none )

        Load val ->
            ( loadColumns model val, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


persist : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
persist ( model, cmd ) =
    ( model
    , if model.env.indexedDBAvailable then
        Cmd.batch [ cmd, Ports.sendToJs (encodeModel model) ]

      else
        cmd
    )


encodeModel : Model -> E.Value
encodeModel { columns } =
    E.object
        [ ( "columns", E.array Column.encoder columns )
        ]


loadColumns : Model -> D.Value -> Model
loadColumns model value =
    case D.decodeValue savedStateDecoder value of
        Ok ((_ :: _) as nonEmptyColumns) ->
            let
                applyId decoded ( accColumns, accIdGen ) =
                    UniqueId.genAndMap "column" accIdGen <|
                        \newId ->
                            { decoded | id = newId } :: accColumns

                ( newColumns, newIdGen ) =
                    List.foldr applyId ( [], model.idGen ) nonEmptyColumns
            in
            { model | columns = Array.fromList newColumns, idGen = newIdGen }

        _ ->
            welcomeModel model.env model.navKey


savedStateDecoder : D.Decoder (List Column)
savedStateDecoder =
    D.field "columns" (D.list Column.decoder)



-- SUB


sub : Model -> Sub Msg
sub _ =
    Sub.batch
        [ onResize Resize
        , Ports.loadFromJs Load
        ]



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Zephyr"
    , body = View.body m
    }



-- MAIN


main : Program Env Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        , onUrlRequest = LinkClicked
        , onUrlChange = \_ -> NoOp
        }
