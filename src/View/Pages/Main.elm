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
import Data.ProducerRegistry as ProducerRegistry
import Dict
import Html exposing (Html)
import View.Organisms.Config.Discord as VDiscord
import View.Organisms.Config.Pref
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

                -- , slack = renderConfigSlack m
                , slack = none
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



-- renderConfigSlack : Model -> Html Msg
-- renderConfigSlack m =
--     View.Organisms.Config.Pref.render
--         { onZephyrModeChange = PrefCtrl Pref.ZephyrMode
--         , onShowColumnButtonClick = ShowColumn
--         , onDeleteColumnButtonClick = DelColumn
--         , onLoggingChange = PrefCtrl Pref.Logging
--         }
--         { zephyrMode = m.pref.zephyrMode
--         , evictThreshold = m.pref.evictThreshold
--         , columnSlotsAvailable = not m.pref.zephyrMode || ColumnStore.sizePinned m.columnStore < m.pref.evictThreshold
--         , shadowColumns = ColumnStore.listShadow m.columnStore
--         , logging = m.pref.logging
--         }


renderConfigDiscord : Model -> Html Msg
renderConfigDiscord m =
    let
        msgTagger =
            ProducerCtrl << ProducerRegistry.DiscordMsg

        hydratedOnce rehydrating pov =
            let
                ( notSubbed, subbed ) =
                    Dict.values pov.channels
                        |> List.sortWith PDiscord.compareByNames
                        |> List.partition (.fetchStatus >> FetchStatus.dormant)

                subbable =
                    List.map (\c -> VDiscord.SubbableChannel c.id c.name c.guildMaybe) notSubbed

                subbed_ =
                    let
                        marshal c =
                            VDiscord.SubbedChannel c.id
                                c.name
                                c.guildMaybe
                                (FetchStatus.fetching c.fetchStatus)
                                (FetchStatus.subscribed c.fetchStatus)
                    in
                    List.map marshal subbed
            in
            VDiscord.hydratedOnce rehydrating pov.user pov.guilds subbable subbed_
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
