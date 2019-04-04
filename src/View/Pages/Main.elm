module View.Pages.Main exposing (render)

import Array
import ArrayExtra
import Broker
import Data.Column as Column
import Data.ColumnStore as ColumnStore
import Data.Filter as Filter
import Data.Model exposing (Model)
import Data.Msg exposing (..)
import Data.Pref as Pref
import Data.Producer.Discord as PDiscord
import Data.Producer.FetchStatus as FetchStatus
import Data.Producer.Slack as PSlack
import Data.ProducerRegistry as ProducerRegistry
import Dict
import Html exposing (Html)
import Url
import View.Organisms.Config.Discord as VDiscord
import View.Organisms.Config.Pref
import View.Organisms.Config.Slack as VSlack
import View.Organisms.Config.Status
import View.Style exposing (none)
import View.Templates.Main


render : Model -> List (Html Msg)
render m =
    let
        effects =
            case m.viewState.columnSwapMaybe of
                Just swap ->
                    { columnDragHover =
                        \newIndex ->
                            DragEnter (ArrayExtra.moveFromTo swap.originalIndex newIndex swap.originalOrder)
                    , columnDragEnd = DragEnd
                    , sidebarEffects = sidebarEffects
                    }

                Nothing ->
                    { columnDragHover = always NoOp
                    , columnDragEnd = NoOp
                    , sidebarEffects = sidebarEffects
                    }

        sidebarEffects =
            { onConfigToggleClick = ToggleConfig
            , onAddColumnClick = AddEmptyColumn
            , onColumnButtonClickByIndex = RevealColumn
            }

        props =
            { configOpen = m.viewState.configOpen
            , visibleColumns = []
            }

        contents =
            { configContents =
                { pref = renderConfigPref m
                , slack = renderConfigSlack m
                , discord = renderConfigDiscord m
                , status = renderConfigStatus m
                }
            , columnContents =
                { header = \index column -> none
                , config = \index column -> none
                , newMessageEditor = \column -> none
                , items = \column -> none
                }
            }
    in
    View.Templates.Main.render effects props contents


renderConfigPref : Model -> Html Msg
renderConfigPref m =
    View.Organisms.Config.Pref.render
        { onZephyrModeChange = PrefCtrl << Pref.ZephyrMode
        , onShowColumnButtonClick = ShowColumn
        , onDeleteColumnButtonClick = DelColumn
        , onLoggingChange = PrefCtrl << Pref.Logging
        }
        { zephyrMode = m.pref.zephyrMode
        , evictThreshold = m.pref.evictThreshold
        , columnSlotsAvailable = not m.pref.zephyrMode || ColumnStore.sizePinned m.columnStore < m.pref.evictThreshold
        , shadowColumns = []
        , logging = m.pref.logging
        }


renderConfigSlack : Model -> Html Msg
renderConfigSlack m =
    let
        msgTagger =
            ProducerCtrl << ProducerRegistry.SlackMsg

        teamStates =
            let
                marshal teamId teamState acc =
                    let
                        pair =
                            case teamState of
                                PSlack.Identified s ->
                                    ( marshalTeam s.team, VSlack.NowHydrating (marshalUser s.user) )

                                PSlack.Hydrated _ pov ->
                                    ( marshalTeam pov.team, hydratedOnce False pov )

                                PSlack.Rehydrating _ pov ->
                                    ( marshalTeam pov.team, hydratedOnce True pov )

                                PSlack.Revisit pov ->
                                    ( marshalTeam pov.team, hydratedOnce True pov )

                                PSlack.Expired _ pov ->
                                    ( marshalTeam pov.team, hydratedOnce False pov )

                        marshalTeam t =
                            VSlack.TeamSnip teamId t.name t.domain <|
                                if t.icon.imageDefault then
                                    Nothing

                                else
                                    Just (Url.toString t.icon.image44)

                        marshalUser u =
                            VSlack.UserSnip u.profile.realName u.profile.displayName (Url.toString u.profile.image48)

                        hydratedOnce rehydrating pov =
                            let
                                ( subbable, subbed ) =
                                    -- TODO support IM/MPIM
                                    Dict.values pov.conversations
                                        |> List.filter (\c -> not c.isArchived && PSlack.isChannel c)
                                        |> List.sortWith PSlack.compareByMembersipThenName
                                        |> List.foldr partitionThenMarshal ( [], [] )

                                partitionThenMarshal c ( accNotSubbed, accSubbed ) =
                                    if FetchStatus.dormant c.fetchStatus then
                                        ( VSlack.SubbableConv (PSlack.getConversationIdStr c)
                                            c.name
                                            (PSlack.isPrivate c)
                                            :: accNotSubbed
                                        , accSubbed
                                        )

                                    else
                                        let
                                            marshalled =
                                                VSlack.SubbedConv (PSlack.getConversationIdStr c)
                                                    c.name
                                                    (PSlack.isPrivate c)
                                                    (FetchStatus.fetching c.fetchStatus)
                                                    (FetchStatus.subscribed c.fetchStatus)
                                        in
                                        ( accNotSubbed, marshalled :: accSubbed )
                            in
                            VSlack.hydratedOnce rehydrating (marshalUser pov.user) subbable subbed
                    in
                    pair :: acc
            in
            Dict.foldr marshal [] m.producerRegistry.slack.dict
    in
    VSlack.render
        { onTokenInput = msgTagger << PSlack.UTokenInput
        , onTokenSubmit = msgTagger PSlack.UTokenCommit
        , onRehydrateButtonClick = msgTagger << PSlack.IRehydrate
        , onConvSelect = \teamId convId -> msgTagger (PSlack.ISubscribe teamId convId)
        , onForceFetchButtonClick = \_ _ -> NoOp -- TODO
        , onCreateColumnButtonClick = AddSimpleColumn << Filter.OfSlackConversation
        , onUnsubscribeButtonClick = \teamId convId -> msgTagger (PSlack.IUnsubscribe teamId convId)
        , selectMsgTagger = SelectCtrl
        }
    <|
        case m.producerRegistry.slack.unidentified of
            PSlack.TokenWritable token ->
                VSlack.Props token True teamStates m.viewState.selectState

            PSlack.TokenIdentifying token ->
                VSlack.Props token False teamStates m.viewState.selectState


renderConfigDiscord : Model -> Html Msg
renderConfigDiscord m =
    let
        msgTagger =
            ProducerCtrl << ProducerRegistry.DiscordMsg

        hydratedOnce rehydrating pov =
            let
                ( subbable, subbed ) =
                    Dict.values pov.channels
                        |> List.sortWith PDiscord.compareByNames
                        |> List.foldr partitionThenMarshal ( [], [] )

                partitionThenMarshal c ( accNotSubbed, accSubbed ) =
                    if FetchStatus.dormant c.fetchStatus then
                        ( VDiscord.SubbableChannel c.id c.name c.guildMaybe :: accNotSubbed, accSubbed )

                    else
                        let
                            marshalled =
                                VDiscord.SubbedChannel c.id
                                    c.name
                                    c.guildMaybe
                                    (FetchStatus.fetching c.fetchStatus)
                                    (FetchStatus.subscribed c.fetchStatus)
                        in
                        ( accNotSubbed, marshalled :: accSubbed )
            in
            VDiscord.hydratedOnce rehydrating pov.user pov.guilds subbable subbed
    in
    VDiscord.render
        { onTokenInput = msgTagger << PDiscord.TokenInput
        , onTokenSubmit = msgTagger PDiscord.TokenCommit
        , onRehydrateButtonClick = msgTagger PDiscord.Rehydrate
        , onChannelSelect = msgTagger << PDiscord.Subscribe
        , onForceFetchButtonClick = always NoOp -- TODO
        , onCreateColumnButtonClick = AddSimpleColumn << Filter.OfDiscordChannel
        , onUnsubscribeButtonClick = msgTagger << PDiscord.Unsubscribe
        , selectMsgTagger = SelectCtrl
        }
    <|
        case m.producerRegistry.discord of
            PDiscord.TokenWritable token ->
                VDiscord.Props token "Register" (not (String.isEmpty token)) VDiscord.NotIdentified m.viewState.selectState

            PDiscord.TokenReady token ->
                VDiscord.Props token "Waiting..." False VDiscord.NotIdentified m.viewState.selectState

            PDiscord.Identified s ->
                VDiscord.Props s.token "Fetching data..." False (VDiscord.NowHydrating s.user) m.viewState.selectState

            PDiscord.Hydrated token pov ->
                let
                    text =
                        if String.isEmpty token then
                            "Unregister"

                        else
                            "Change Token"
                in
                VDiscord.Props token text (token /= pov.token) (hydratedOnce False pov) m.viewState.selectState

            PDiscord.Rehydrating token pov ->
                VDiscord.Props token "Fetching data..." False (hydratedOnce True pov) m.viewState.selectState

            PDiscord.Revisit pov ->
                VDiscord.Props pov.token "Reloading..." False (hydratedOnce True pov) m.viewState.selectState

            PDiscord.Expired token pov ->
                let
                    text =
                        if token == pov.token then
                            "Expired"

                        else
                            "Change Token"
                in
                VDiscord.Props token text (token /= pov.token) (hydratedOnce False pov) m.viewState.selectState

            PDiscord.Switching s pov ->
                VDiscord.Props s.token "Switching user..." False (hydratedOnce False pov) m.viewState.selectState


renderConfigStatus : Model -> Html Msg
renderConfigStatus m =
    View.Organisms.Config.Status.render
        { itemBrokerCapacity = Broker.capacity m.itemBroker
        , columnItemLimit = Column.columnItemLimit
        , numColumns = ColumnStore.size m.columnStore
        , numVisible = Array.length m.columnStore.order
        , numPinned = ColumnStore.sizePinned m.columnStore
        , clientHeight = m.env.clientHeight
        , clientWidth = m.env.clientWidth
        , serviceWorkerAvailable = m.env.serviceWorkerAvailable
        , indexedDBAvailable = m.env.indexedDBAvailable
        }
