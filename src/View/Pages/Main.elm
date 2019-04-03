module View.Pages.Main exposing (render)

import Array
import ArrayExtra
import Broker
import Data.Column as Column
import Data.ColumnStore as ColumnStore
import Data.Model exposing (Model)
import Data.Msg exposing (..)
import Data.Pref as Pref
import Data.Producer.Discord as Discord
import Html exposing (Html)
import View.Organisms.Config.Discord
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
                -- , discord = renderConfigDiscord m
                , slack = none
                , discord = none
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
--
--
-- renderConfigDiscord : Model -> Html Msg
-- renderConfigDiscord m =
--     View.Organisms.Config.Discord.render
--         { onTokenInput = ProducerCtrl << Discord.TokenInput
--         , onTokenSubmit = ProducerCtrl Discord.TokenCommit
--         , onRehydrateButtonClick = ProducerCtrl Discord.Rehydrate
--         , onChannelSelect = ProducerCtrl << Discord.Subscribe
--         , selectMsgTagger = SelectCtrl
--         , onForceFetchButtonClick = always NoOp
--         , onCreateColumnButtonClick = always NoOp
--         , onUnsubscribeButtonClick = always NoOp
--         }
--         { token = m.textInput
--         , tokenSubmitButtonText = "Submit"
--         , tokenSubmittable = True
--         , currentState = m.producerRegistry.discord
--         , selectState = m.viewState.selectState
--         }


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
