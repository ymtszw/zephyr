module Data.Msg exposing (Msg(..), logEntry)

import Array exposing (Array)
import Broker exposing (Broker)
import Browser
import Browser.Dom
import Data.Column as Column
import Data.ColumnStore exposing (ColumnStore)
import Data.Filter as Filter
import Data.Item exposing (Item)
import Data.Pref exposing (Pref)
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Data.ProducerRegistry as ProducerRegistry exposing (ProducerRegistry)
import Data.SavedState exposing (SavedState)
import Data.UniqueIdGen exposing (UniqueIdGen)
import File
import HttpClient
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Logger
import Logger.Entry exposing (Entry)
import Scroll
import String exposing (fromInt)
import Time exposing (Posix, Zone)
import Url


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Browser.Dom.Viewport
    | GetTimeZone ( String, Zone )
    | VisibilityChanged Bool
    | LoggerCtrl Logger.Msg
    | LinkClicked Browser.UrlRequest
    | SelectToggle String Bool
    | SelectPick Msg
    | AddEmptyColumn
    | AddSimpleColumn Filter.FilterAtom
    | DelColumn String
    | DismissColumn Int
    | ShowColumn String
    | DragStart { index : Int, id : String, pinned : Bool }
    | DragEnter (Array String)
    | DragEnd
    | LoadColumnStore ( ColumnStore, UniqueIdGen, Cmd Msg )
    | LoadItemBroker (Broker Item)
    | LoadProducerRegistry ProducerRegistry
    | LoadPref Pref
    | LoadOk SavedState
    | LoadErr D.Error
    | ToggleConfig Bool
    | ColumnCtrl String Column.Msg
    | ProducerCtrl ProducerRegistry.Msg
    | RevealColumn Int
    | DomOp (Result Browser.Dom.Error ())
    | ZephyrMode Bool
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

        VisibilityChanged visible ->
            if visible then
                Entry "VisibilityChanged" [ "Visible" ]

            else
                Entry "VisibilityChanged" [ "Hidden" ]

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

        DelColumn cId ->
            Entry "DelColumn" [ cId ]

        DismissColumn index ->
            Entry "DismissColumn" [ fromInt index ]

        ShowColumn cId ->
            Entry "ShowColumn" [ cId ]

        DragStart { index, id, pinned } ->
            Entry "DragStart" [ fromInt index, id, boolStr pinned ]

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

        LoadPref { zephyrMode, evictThreshold } ->
            Entry "LoadPref"
                [ "zephyrMode: " ++ boolStr zephyrMode
                , "evictThreshold: " ++ String.fromInt evictThreshold
                ]

        LoadOk _ ->
            -- Old gigantic state load; remove after migration
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

        ZephyrMode bool ->
            Entry "ZephyrMode" [ boolStr bool ]

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

        Scroll.Reveal ->
            Entry (prefix ++ ".Reveal") []

        Scroll.NewItem ->
            Entry (prefix ++ ".NewItem") []

        Scroll.LoadMore ->
            Entry (prefix ++ ".LoadMore") []

        Scroll.AdjustReq boundingHeight ->
            Entry (prefix ++ ".AdjustReq") [ String.fromInt boundingHeight ]

        Scroll.AdjustExec boundingHeight vp ->
            Entry (prefix ++ ".AdjustExec") [ String.fromInt boundingHeight, viewportToString vp ]


producerMsgToEntry : ProducerRegistry.Msg -> Entry
producerMsgToEntry pMsg =
    case pMsg of
        ProducerRegistry.DiscordMsg msgDiscord ->
            case msgDiscord of
                Discord.TokenInput input ->
                    Entry "Discord.TokenInput" [ input ]

                Discord.TokenCommit ->
                    Entry "Discord.TokenCommit" []

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

                Discord.Fetched succ ->
                    Entry "Discord.Fetched"
                        [ succ.channelId
                        , E.encode 2 (E.list Discord.encodeMessage succ.messages)
                        , Iso8601.fromTime succ.posix
                        ]

                Discord.Post opts ->
                    Entry "Discord.Post" [ opts.channelId ]

                Discord.Posted succ ->
                    Entry "Discord.Posted" [ succ.channelId, Iso8601.fromTime succ.posix ]

                Discord.ChannelAPIError cId ( e, req ) ->
                    Entry "Discord.ChannelAPIError"
                        [ cId
                        , HttpClient.errorToString e
                        , req.method
                        , Url.toString req.url
                        ]

                Discord.GenericAPIError ( e, req ) ->
                    Entry "Discord.GenericAPIError"
                        [ HttpClient.errorToString e
                        , req.method
                        , Url.toString req.url
                        ]

        ProducerRegistry.SlackMsg sMsg ->
            case sMsg of
                Slack.UTokenInput t ->
                    Entry "Slack.UTokenInput" [ t ]

                Slack.UTokenCommit ->
                    Entry "Slack.UTokenCommit" []

                Slack.UAPIFailure f ->
                    Entry "Slack.UAPIFailure" <|
                        case f of
                            Slack.HttpFailure ( e, req ) ->
                                [ HttpClient.errorToString e
                                , req.method
                                , Url.toString req.url
                                ]

                            Slack.RpcError e ->
                                [ e ]

                Slack.Identify user team ->
                    Entry "Slack.Identify"
                        [ E.encode 2 (Slack.encodeUser user)
                        , E.encode 2 (Slack.encodeTeam team)
                        ]


columnMsgToEntry : String -> Column.Msg -> Entry
columnMsgToEntry cId cMsg =
    case cMsg of
        Column.ToggleConfig bool ->
            Entry "Column.ToggleConfig" [ cId, boolStr bool ]

        Column.Pin bool ->
            Entry "Column.Pin" [ cId, boolStr bool ]

        Column.Calm ->
            Entry "Column.Calm" [ cId ]

        Column.Show ->
            Entry "Column.Show" [ cId ]

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

        Column.SelectEditor index ->
            Entry "Column.SelectEditor" [ cId, String.fromInt index ]

        Column.EditorToggle isActive ->
            Entry "Column.EditorToggle" [ cId, boolStr isActive ]

        Column.EditorInput input ->
            Entry "Column.EditorInput" [ cId, input ]

        Column.EditorReset ->
            Entry "Column.EditorReset" [ cId ]

        Column.EditorSubmit ->
            Entry "Column.EditorSubmit" [ cId ]

        Column.EditorFileRequest mimeTypes ->
            Entry "Column.EditorFileRequest" [ cId, String.join "," mimeTypes ]

        Column.EditorFileSelected file ->
            Entry "Column.EditorFileSelected" [ cId, File.name file, File.mime file ]

        Column.EditorFileLoaded ( file, _ ) ->
            Entry "Column.EditorFileLoaded" [ cId, File.name file, File.mime file ]

        Column.EditorFileDiscard ->
            Entry "Column.EditorFileDiscard" [ cId ]

        Column.ScanBroker { maxCount } ->
            Entry "Column.ScanBroker" [ cId, String.fromInt maxCount ]

        Column.ScrollMsg sMsg ->
            scrollMsgToEntry "Column" sMsg
