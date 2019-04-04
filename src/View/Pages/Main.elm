module View.Pages.Main exposing (render)

import Array exposing (Array)
import ArrayExtra
import Broker
import Data.Column as Column
import Data.ColumnStore as ColumnStore
import Data.Filter as Filter exposing (Filter)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial, findDiscordChannel, findSlackConversation)
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
import View.Molecules.Source as Source exposing (Source)
import View.Organisms.Config.Discord as VDiscord
import View.Organisms.Config.Pref
import View.Organisms.Config.Slack as VSlack
import View.Organisms.Config.Status
import View.Style exposing (none)
import View.Templates.Main exposing (DragStatus(..))


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
            let
                -- { id : String
                -- , items : Scroll ColumnItem
                -- , filters : Array Filter
                -- , offset : Maybe Offset
                -- , pinned : Bool
                -- , recentlyTouched : Bool -- This property may become stale, though it should have no harm
                -- , configOpen : Bool
                -- , pendingFilters : Array Filter
                -- , editors : SelectArray ColumnEditor
                -- , editorSeq : Int -- Force triggering DOM generation when incremented; workaround for https://github.com/mdgriffith/elm-ui/issues/5
                -- , editorActive : Bool
                -- , deleteGate : String
                marshalVisibleColumn fam index c =
                    let
                        ( sources, filters ) =
                            marshalSourcesAndFilters m.columnStore.fam c.filters

                        dragStatus =
                            case m.viewState.columnSwapMaybe of
                                Just swap ->
                                    if swap.grabbedId == c.id then
                                        Grabbed

                                    else if swap.pinned == c.pinned then
                                        Droppable

                                    else
                                        Undroppable

                                Nothing ->
                                    Settled
                    in
                    { id = c.id, pinned = c.pinned, sources = sources, filters = filters, dragStatus = dragStatus, configOpen = c.configOpen }
            in
            { configOpen = m.viewState.configOpen
            , visibleColumns = ColumnStore.mapForView marshalVisibleColumn m.columnStore
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


marshalSourcesAndFilters : FilterAtomMaterial -> Array Filter -> ( List Source, List String )
marshalSourcesAndFilters fam filters =
    let
        collectSourceAndFilter f ( accSoures, accFilters ) =
            Tuple.mapBoth ((++) accSoures) ((++) accFilters) (Filter.foldl findSourceOrFilter ( [], [] ) f)

        findSourceOrFilter fa ( accSources, accFilters ) =
            case fa of
                Filter.OfDiscordChannel channelId ->
                    let
                        withGuild =
                            Maybe.andThen (\c -> Maybe.map (Tuple.pair c) c.guildMaybe)
                    in
                    -- TODO support DMs
                    case withGuild (findDiscordChannel channelId fam) of
                        Just ( c, g ) ->
                            ( Source.discord channelId
                                c.name
                                g.name
                                (Maybe.map (PDiscord.imageUrlNoFallback (Just Source.desiredIconSize)) g.icon)
                                :: accSources
                            , accFilters
                            )

                        Nothing ->
                            ( accSources, accFilters )

                Filter.OfSlackConversation convId ->
                    -- TODO support IM/MPIMs
                    case findSlackConversation convId fam of
                        Just c ->
                            ( Source.slack convId c.name c.team.name (teamIcon44 c.team) (PSlack.isPrivate c) :: accSources, accFilters )

                        Nothing ->
                            ( accSources, accFilters )

                Filter.ByMessage text ->
                    -- Wrap in extra quotation
                    ( accSources, ("\"" ++ text ++ "\"") :: accFilters )

                Filter.ByMedia Filter.HasImage ->
                    ( accSources, "Has Image" :: accFilters )

                Filter.ByMedia Filter.HasVideo ->
                    ( accSources, "Has Video" :: accFilters )

                Filter.ByMedia Filter.HasNone ->
                    ( accSources, "Without Media" :: accFilters )

                Filter.RemoveMe ->
                    ( accSources, accFilters )
    in
    Array.foldl collectSourceAndFilter ( [], [] ) filters


renderConfigPref : Model -> Html Msg
renderConfigPref m =
    let
        marshalShadowColumn c =
            let
                ( sources, filters ) =
                    marshalSourcesAndFilters m.columnStore.fam c.filters
            in
            { id = c.id, pinned = c.pinned, sources = sources, filters = filters }
    in
    View.Organisms.Config.Pref.render
        { onZephyrModeChange = PrefCtrl << Pref.ZephyrMode
        , onShowColumnButtonClick = ShowColumn
        , onDeleteColumnButtonClick = DelColumn
        , onLoggingChange = PrefCtrl << Pref.Logging
        }
        { zephyrMode = m.pref.zephyrMode
        , evictThreshold = m.pref.evictThreshold
        , columnSlotsAvailable = not m.pref.zephyrMode || ColumnStore.sizePinned m.columnStore < m.pref.evictThreshold
        , shadowColumns = List.map marshalShadowColumn (ColumnStore.listShadow m.columnStore)
        , logging = m.pref.logging
        }


teamIcon44 : PSlack.Team -> Maybe String
teamIcon44 t =
    if t.icon.imageDefault then
        Nothing

    else
        Just (Url.toString t.icon.image44)


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
                            VSlack.TeamSnip teamId t.name t.domain (teamIcon44 t)

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
