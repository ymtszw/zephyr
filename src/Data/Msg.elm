module Data.Msg exposing (Msg(..), logEntry)

import Array exposing (Array)
import Broker exposing (Broker)
import Browser
import Browser.Dom
import Data.Column as Column
import Data.ColumnStore exposing (ColumnStore)
import Data.Filter as Filter
import Data.Item exposing (Item)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.SavedState exposing (SavedState)
import Data.UniqueIdGen exposing (UniqueIdGen)
import HttpExtra
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Logger exposing (Entry)
import Scroll
import String exposing (fromInt)
import Time exposing (Posix, Zone)
import Url


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Browser.Dom.Viewport
    | GetTimeZone ( String, Zone )
    | LoggerCtrl Logger.Msg
    | LinkClicked Browser.UrlRequest
    | SelectToggle String Bool
    | SelectPick Msg
    | AddEmptyColumn
    | AddSimpleColumn Filter.FilterAtom
    | DelColumn Int
    | ToggleColumnSwappable Bool
    | DragStart Int String
    | DragEnter (Array String)
    | DragEnd
    | LoadColumnStore ( ColumnStore, UniqueIdGen )
    | LoadItemBroker (Broker Item)
    | LoadProducerRegistry ProducerRegistry
    | LoadOk SavedState
    | LoadErr D.Error
    | ToggleConfig Bool
    | ColumnCtrl String Column.Msg
    | ProducerCtrl Producer.Msg
    | RevealColumn Int
    | DomOp (Result Browser.Dom.Error ())
    | Tick Posix


logEntry : Msg -> Entry
logEntry msg =
    case msg of
        NoOp ->
            Entry "NoOp" []

        Resize x y ->
            Entry "Resize" [ fromInt x, fromInt y ]

        GetViewport vp ->
            Entry "GetViewport" [ viewportToString vp ]

        GetTimeZone ( name, _ ) ->
            Entry "GetTimeZone" [ name ]

        LoggerCtrl lMsg ->
            loggerMsgToEntry lMsg

        LinkClicked (Browser.Internal url) ->
            Entry "LinkClicked.Internal" [ Url.toString url ]

        LinkClicked (Browser.External str) ->
            Entry "LinkClicked.External" [ str ]

        SelectToggle sId bool ->
            Entry "SelectToggle" [ sId, boolStr bool ]

        SelectPick sMsg ->
            logEntry sMsg

        AddEmptyColumn ->
            Entry "AddEmptyColumn" []

        AddSimpleColumn fa ->
            Entry "AddSimpleColumn" [ Filter.atomToString fa ]

        DelColumn index ->
            Entry "DelColumn" [ fromInt index ]

        ToggleColumnSwappable bool ->
            Entry "ToggleColumnSwappable" [ boolStr bool ]

        DragStart index cId ->
            Entry "DragStart" [ fromInt index, cId ]

        DragEnter order ->
            Entry "DragEnter" [ String.join "," (Array.toList order) ]

        DragEnd ->
            Entry "DragEnd" []

        LoadColumnStore _ ->
            Entry "LoadColumnStore" [ "<columnStore>" ]

        LoadItemBroker _ ->
            Entry "LoadItemBroker" [ "<itemBroker>" ]

        LoadProducerRegistry _ ->
            Entry "LoadProducerRegistry" [ "<producerRegistry>" ]

        LoadOk _ ->
            Entry "LoadOk" [ "<savedState>" ]

        LoadErr e ->
            Entry "LoadErr" [ D.errorToString e ]

        ToggleConfig bool ->
            Entry "ToggleConfig" [ boolStr bool ]

        ColumnCtrl cId cMsg ->
            columnMsgToEntry cId cMsg

        ProducerCtrl pMsg ->
            producerMsgToEntry pMsg

        RevealColumn index ->
            Entry "Reveal" [ fromInt index ]

        DomOp (Ok ()) ->
            Entry "DomOp.Ok" []

        DomOp (Err (Browser.Dom.NotFound id)) ->
            Entry "DomOp.Err.NotFound" [ id ]

        Tick posix ->
            Entry "Tick" [ Iso8601.fromTime posix ]


boolStr : Bool -> String
boolStr b =
    if b then
        "True"

    else
        "False"


viewportToString : Browser.Dom.Viewport -> String
viewportToString vp =
    E.encode 2 <|
        E.object
            [ Tuple.pair "scene" <|
                E.object
                    [ ( "width", E.float vp.scene.width )
                    , ( "height", E.float vp.scene.height )
                    ]
            , Tuple.pair "viewport" <|
                E.object
                    [ ( "width", E.float vp.viewport.width )
                    , ( "height", E.float vp.viewport.height )
                    , ( "x", E.float vp.viewport.x )
                    , ( "y", E.float vp.viewport.y )
                    ]
            ]


loggerMsgToEntry : Logger.Msg -> Entry
loggerMsgToEntry lMsg =
    let
        filterMode isPos =
            if isPos then
                "Include: "

            else
                "Exclude: "
    in
    case lMsg of
        Logger.ScrollMsg sMsg ->
            scrollMsgToEntry "Logger" sMsg

        Logger.FilterInput query ->
            Entry "Logger.FilterInput" [ query ]

        Logger.SetMsgFilter (Logger.MsgFilter isPos ctor) ->
            Entry "Logger.SetMsgFilter" [ filterMode isPos ++ ctor ]

        Logger.DelMsgFilter (Logger.MsgFilter isPos ctor) ->
            Entry "Logger.DelMsgFilter" [ filterMode isPos ++ ctor ]


scrollMsgToEntry : String -> Scroll.Msg -> Entry
scrollMsgToEntry prefix sMsg =
    case sMsg of
        Scroll.ScrollStart ->
            Entry (prefix ++ ".ScrollStart") []

        Scroll.ViewportResult (Ok vp) ->
            Entry (prefix ++ ".ViewportOk") [ viewportToString vp ]

        Scroll.ViewportResult (Err _) ->
            Entry (prefix ++ ".ViewportNotFound") []

        Scroll.BackToTop ->
            Entry (prefix ++ ".BackToTop") []


producerMsgToEntry : Producer.Msg -> Entry
producerMsgToEntry pMsg =
    case pMsg of
        Producer.DiscordMsg msgDiscord ->
            case msgDiscord of
                Discord.TokenInput input ->
                    Entry "Discord.TokenInput" [ input ]

                Discord.CommitToken ->
                    Entry "Discord.CommitToken" []

                Discord.Identify user ->
                    Entry "Discord.Identify" [ E.encode 2 (Discord.encodeUser user) ]

                Discord.Hydrate _ _ ->
                    Entry "Discord.Hydrate" [ "<Hydrate>" ]

                Discord.Rehydrate ->
                    Entry "Discord.Rehydrate" []

                Discord.Subscribe cId ->
                    Entry "Discord.Subscribe" [ cId ]

                Discord.Unsubscribe cId ->
                    Entry "Discord.Unsubscribe" [ cId ]

                Discord.Fetch posix ->
                    Entry "Discord.Fetch" [ Iso8601.fromTime posix ]

                Discord.Fetched (Discord.FetchOk cId ms posix) ->
                    Entry "Discord.FetchOk" [ cId, Iso8601.fromTime posix, E.encode 2 (E.list Discord.encodeMessage ms) ]

                Discord.Fetched (Discord.FetchErr cId e) ->
                    Entry "Discord.FetchErr" [ cId, HttpExtra.errorToString e ]

                Discord.APIError e ->
                    Entry "Discord.APIError" [ HttpExtra.errorToString e ]


columnMsgToEntry : String -> Column.Msg -> Entry
columnMsgToEntry cId cMsg =
    case cMsg of
        Column.ToggleConfig bool ->
            Entry "Column.ToggleConfig" [ cId, boolStr bool ]

        Column.AddFilter filter ->
            Entry "Column.AddFilter" [ cId, Filter.toString filter ]

        Column.DelFilter index ->
            Entry "Column.DelFilter" [ cId, fromInt index ]

        Column.AddFilterAtom { filterIndex, atom } ->
            Entry "Column.AddFilterAtom" [ cId, fromInt filterIndex, Filter.atomToString atom ]

        Column.SetFilterAtom { filterIndex, atomIndex, atom } ->
            Entry "Column.SetFilterAtom" [ cId, fromInt filterIndex, fromInt atomIndex, Filter.atomToString atom ]

        Column.DelFilterAtom { filterIndex, atomIndex } ->
            Entry "Column.DelFilterAtom" [ cId, fromInt filterIndex, fromInt atomIndex ]

        Column.ConfirmFilter ->
            Entry "Column.ConfirmFilter" []

        Column.DeleteGateInput input ->
            Entry "Column.DeleteGateInput" [ cId, input ]

        Column.ScrollMsg sMsg ->
            scrollMsgToEntry "Column" sMsg
