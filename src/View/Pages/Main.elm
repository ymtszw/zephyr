module View.Pages.Main exposing (render)

import Array exposing (Array)
import ArrayExtra
import Broker
import Color exposing (Color)
import Data.Column as Column
import Data.ColumnItem as ColumnItem
import Data.ColumnItem.Contents exposing (..)
import Data.ColumnItem.EmbeddedMatter as EmbeddedMatter
import Data.ColumnItem.NamedEntity as NamedEntity
import Data.ColumnStore as ColumnStore
import Data.Filter as Filter exposing (Filter)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial, findDiscordChannel, findSlackConversation)
import Data.Item
import Data.Model exposing (Model)
import Data.Msg exposing (..)
import Data.Pref as Pref
import Data.Producer.Discord as PDiscord
import Data.Producer.FetchStatus as FetchStatus
import Data.Producer.Slack as PSlack
import Data.ProducerRegistry as ProducerRegistry
import Dict
import Element
import Html exposing (Html)
import List.Extra
import Scroll
import TimeExtra exposing (ms)
import Url
import View.Molecules.Source as Source exposing (Source)
import View.Organisms.Column.Config
import View.Organisms.Column.Header
import View.Organisms.Column.Items
import View.Organisms.Column.NewMessageEditor
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
            let
                ( onColumnDragHover, onColumnDragEnd ) =
                    case m.viewState.columnSwapMaybe of
                        Just swap ->
                            ( \newIndex ->
                                DragEnter (ArrayExtra.moveFromTo swap.originalIndex newIndex swap.originalOrder)
                            , DragEnd
                            )

                        Nothing ->
                            ( always NoOp, NoOp )
            in
            { onColumnDragHover = onColumnDragHover
            , onColumnDragEnd = onColumnDragEnd
            , onColumnBorderFlashEnd = \cId -> ColumnCtrl cId Column.Calm
            , columnItemsScrollAttrs =
                \c ->
                    Scroll.scrollAttrs (ColumnCtrl c.id << Column.ScrollMsg) c.items
            , sidebarEffects = sidebarEffects
            }

        sidebarEffects =
            { onConfigToggleClick = ToggleConfig
            , onAddColumnClick = AddEmptyColumn
            , onColumnButtonClickByIndex = RevealColumn
            }

        props =
            let
                marshalVisibleColumn fam index c =
                    let
                        ( sources, filters ) =
                            marshalSourcesAndFilters m.columnStore.fam c.pendingFilters

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
                    { id = c.id
                    , pinned = c.pinned
                    , sources = sources
                    , filters = filters
                    , dragStatus = dragStatus
                    , configOpen = c.configOpen
                    , scrolled = Scroll.scrolled c.items
                    , numItems = Scroll.size c.items
                    , items = c.items
                    , recentlyTouched = c.recentlyTouched
                    , editors = c.editors
                    , editorSeq = c.editorSeq
                    , userActionOnEditor = c.userActionOnEditor
                    }
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
                { header =
                    View.Organisms.Column.Header.render
                        { onColumnDragStart = \pinned index_ id -> DragStart { id = id, index = index_, pinned = pinned }
                        , onHeaderClickWhenScrolled = \cId -> ColumnCtrl cId (Column.ScrollMsg Scroll.BackToTop)
                        , onPinButtonClick = \cId pinned -> ColumnCtrl cId (Column.Pin pinned)
                        , onConfigToggleButtonClick = \cId open -> ColumnCtrl cId (Column.ToggleConfig open)
                        , onDismissButtonClick = DismissColumn
                        }
                , config =
                    \index c ->
                        View.Organisms.Column.Config.render
                            { onCloseButtonClick = \cId -> ColumnCtrl cId (Column.ToggleConfig False)
                            , onColumnDeleteButtonClick = DelColumn
                            , onSourceSelect =
                                \cId source ->
                                    -- TODO Use brandnew & precise source update Msg
                                    -- Currently it is not working properly!!
                                    -- case source of
                                    --     Source.DiscordSource opts ->
                                    --         ColumnCtrl cId (Column.AddFilterAtom { filterIndex = 0, atom = Filter.OfDiscordChannel opts.id })
                                    --
                                    --     Source.SlackSource opts ->
                                    --         ColumnCtrl cId (Column.AddFilterAtom { filterIndex = 0, atom = Filter.OfSlackConversation opts.id })
                                    NoOp
                            , selectMsgTagger = SelectCtrl
                            , onRemoveSourceButtonClick =
                                \cId atomIndex ->
                                    -- TODO Use brandnew & precise source update Msg
                                    -- ColumnCtrl cId (Column.DelFilterAtom { filterIndex = 0, atomIndex = atomIndex })
                                    NoOp
                            }
                            { selectState = m.viewState.selectState
                            , availableSourecs = List.filter (\s -> not (List.member s c.sources)) availableSources
                            , column = c
                            }
                , newMessageEditor =
                    \c ->
                        View.Organisms.Column.NewMessageEditor.render
                            { onEditorSelect = \cId -> ColumnCtrl cId << Column.SelectEditor
                            , selectMsgTagger = SelectCtrl
                            , onTextInput = \cId -> ColumnCtrl cId << Column.EditorInput
                            , onInteracted = \cId -> ColumnCtrl cId << Column.EditorInteracted
                            , onResetButtonClick = \cId -> ColumnCtrl cId Column.EditorReset
                            , onDiscardFileButtonClick = \cId -> ColumnCtrl cId Column.EditorFileDiscard
                            , onRequestFileAreaClick = \cId -> ColumnCtrl cId (Column.EditorFileRequest [ "*/*" ])
                            , onFileDrop = \cId -> ColumnCtrl cId << Column.EditorFileSelected
                            , onSubmit = \cId -> ColumnCtrl cId Column.EditorSubmit
                            }
                            { selectState = m.viewState.selectState, column = c }
                , items =
                    \c ->
                        let
                            itemsVisible =
                                Scroll.toList c.items
                        in
                        View.Organisms.Column.Items.render
                            { onLoadMoreClick = ColumnCtrl c.id (Column.ScrollMsg Scroll.LoadMore)
                            }
                            { timezone = m.viewState.timezone
                            , itemGroups =
                                -- Do note that items are sorted from "newest to oldest" at the moment it came out from Scrolls.
                                -- Then we reverse, since we want to group items in "older to newer" order, while gloabally showing "newest to oldest"
                                List.Extra.reverseMap marshalColumnItem itemsVisible
                                    |> List.Extra.groupWhile shouldGroupColumnItem
                                    |> List.reverse
                            , hasMore = List.length itemsVisible < Scroll.size c.items
                            }
                }
            }

        availableSources =
            List.concat
                [ case m.columnStore.fam.ofDiscordChannel of
                    Just ( _, channels ) ->
                        let
                            marshalWithGuild c =
                                Maybe.map
                                    (\g ->
                                        Source.discord c.id
                                            c.name
                                            g.name
                                            (Maybe.map (PDiscord.imageUrlNoFallback (Just Source.desiredIconSize)) g.icon)
                                    )
                                    c.guildMaybe
                        in
                        List.filterMap marshalWithGuild channels

                    Nothing ->
                        []
                , case m.columnStore.fam.ofSlackConversation of
                    Just { conversations } ->
                        let
                            marshal c =
                                Source.slack (PSlack.getConversationIdStr c) c.name c.team.name (teamIcon44 c.team) (PSlack.isPrivate c)
                        in
                        List.map marshal conversations

                    Nothing ->
                        []
                ]
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


marshalColumnItem : Column.ColumnItem -> ColumnItem.ColumnItem
marshalColumnItem item =
    case item of
        Column.Product offset (Data.Item.DiscordItem message) ->
            marshalDiscordMessage (Broker.offsetToString offset) message

        Column.Product offset (Data.Item.SlackItem message) ->
            marshalSlackMessage (Broker.offsetToString offset) message

        Column.SystemMessage sm ->
            let
                marshalMedia media =
                    case media of
                        Column.Image url ->
                            ColumnItem.attachedFiles [ attachedImage (Url.toString url) ]

                        Column.Video url ->
                            ColumnItem.attachedFiles [ attachedVideo (Url.toString url) ]
            in
            ColumnItem.new sm.id (NamedEntity.new "System Message") (Markdown sm.message)
                |> apOrId marshalMedia sm.mediaMaybe

        Column.LocalMessage lm ->
            ColumnItem.new lm.id (NamedEntity.new "Memo") (Markdown lm.message)


apOrId : (a -> b -> b) -> Maybe a -> b -> b
apOrId toFunc =
    Maybe.withDefault identity << Maybe.map toFunc


shouldGroupColumnItem : ColumnItem.ColumnItem -> ColumnItem.ColumnItem -> Bool
shouldGroupColumnItem older newer =
    let
        sourceIdsMatch =
            -- (newer.sourceId == older.sourceId) TODO add sourceId to ColumnItem
            True

        authorsMatch =
            newer.author == older.author

        timestampsAreClose =
            case ( older.timestamp, newer.timestamp ) of
                ( Just tsOld, Just tsNew ) ->
                    ms tsOld + groupingIntervalMillis > ms tsNew

                _ ->
                    False

        groupingIntervalMillis =
            60000
    in
    sourceIdsMatch && authorsMatch && timestampsAreClose


marshalDiscordMessage : String -> PDiscord.Message -> ColumnItem.ColumnItem
marshalDiscordMessage id m =
    -- XXX Possibly, m.id can be used for interaction handler
    let
        author =
            let
                authorImpl isBot u =
                    NamedEntity.new u.username
                        |> NamedEntity.secondaryName ("#" ++ u.discriminator)
                        |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (avatarSrc u)) u.username isBot)

                avatarSrc u =
                    PDiscord.imageUrlWithFallback (Just NamedEntity.desiredIconSize) u.discriminator u.avatar
            in
            case m.author of
                PDiscord.UserAuthor u ->
                    authorImpl False u

                PDiscord.WebhookAuthor u ->
                    authorImpl True u

        marshalEmbed e =
            let
                marshalAuthor eAuthor =
                    let
                        marshalIcon url =
                            NamedEntity.imageOrAbbr (Just (Url.toString url)) eAuthor.name False
                    in
                    NamedEntity.new eAuthor.name
                        |> apOrId (Url.toString >> NamedEntity.url) eAuthor.url
                        |> apOrId (marshalIcon >> NamedEntity.avatar) eAuthor.proxyIconUrl

                marshalEmbedImage linkMaybe eImage =
                    imageMedia (Url.toString eImage.url)
                        (Url.toString (Maybe.withDefault eImage.url linkMaybe))
                        "Embedded image"
                        (Maybe.map2 dimension eImage.width eImage.height)

                attachedFiles =
                    List.filterMap identity
                        [ Maybe.map (marshalEmbedImage Nothing >> VisualFile) e.image
                        , Maybe.map
                            (\v ->
                                attachedVideo (Url.toString v.url)
                                    |> apOrId attachedFileDimension (Maybe.map2 dimension v.width v.height)
                            )
                            e.video
                        ]
            in
            EmbeddedMatter.new (Markdown (Maybe.withDefault "" e.description))
                |> apOrId (Plain >> EmbeddedMatter.title) e.title
                |> apOrId (Url.toString >> EmbeddedMatter.url) e.url
                |> apOrId (marshalColor >> EmbeddedMatter.color) e.color
                |> apOrId (marshalAuthor >> EmbeddedMatter.author) e.author
                |> apOrId (marshalEmbedImage e.url >> EmbeddedMatter.thumbnail) e.thumbnail
                |> EmbeddedMatter.attachedFiles attachedFiles

        marshalAttachment a =
            if Data.Item.extIsImage a.filename then
                attachedImage (Url.toString a.proxyUrl)
                    |> attachedFileLink (Url.toString a.url)
                    |> attachedFileDescription a.filename
                    |> apOrId attachedFileDimension (Maybe.map2 dimension a.width a.height)

            else if Data.Item.extIsVideo a.filename then
                attachedVideo (Url.toString a.proxyUrl)
                    |> attachedFileLink (Url.toString a.url)
                    |> attachedFileDescription a.filename
                    |> apOrId attachedFileDimension (Maybe.map2 dimension a.width a.height)

            else
                attachedOther (DownloadUrl (Url.toString a.proxyUrl))
                    |> attachedFileDescription a.filename
    in
    ColumnItem.new id author (Markdown m.content)
        |> ColumnItem.timestamp m.timestamp
        |> ColumnItem.attachedFiles (List.map marshalAttachment m.attachments)
        |> ColumnItem.embeddedMatters (List.map marshalEmbed m.embeds)


marshalColor : Element.Color -> Color
marshalColor =
    Element.toRgb >> Color.fromRgba


dimension : Int -> Int -> { width : Int, height : Int }
dimension w h =
    { width = w, height = h }


marshalSlackMessage : String -> PSlack.Message -> ColumnItem.ColumnItem
marshalSlackMessage id m =
    let
        author =
            case m.author of
                PSlack.UserAuthor u ->
                    let
                        username =
                            Maybe.withDefault u.profile.realName u.profile.displayName
                    in
                    NamedEntity.new username
                        |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Url.toString u.profile.image48)) username False)

                PSlack.UserAuthorId (PSlack.UserId str) ->
                    NamedEntity.new str

                PSlack.BotAuthor b ->
                    let
                        username =
                            Maybe.withDefault b.name m.username
                    in
                    NamedEntity.new username
                        |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Url.toString b.icons.image48)) username True)

                PSlack.BotAuthorId (PSlack.BotId str) ->
                    NamedEntity.new str
                        |> NamedEntity.avatar (NamedEntity.imageOrAbbr Nothing str True)

        marshalAttachment a =
            let
                marshalTitle aTitle =
                    Markdown <|
                        case aTitle.link of
                            Just url ->
                                "[" ++ aTitle.name ++ "](" ++ Url.toString url ++ ")"

                            Nothing ->
                                aTitle.name

                marshalAuthor aAuthor =
                    let
                        marshalIcon url =
                            NamedEntity.imageOrAbbr (Just (Url.toString url)) aAuthor.name False
                    in
                    NamedEntity.new aAuthor.name
                        |> apOrId (Url.toString >> NamedEntity.url) aAuthor.link
                        |> apOrId (marshalIcon >> NamedEntity.avatar) aAuthor.icon

                marshalImageUrl linkMaybe url =
                    imageMedia (Url.toString url) (Url.toString (Maybe.withDefault url linkMaybe)) "Embedded image" Nothing
            in
            EmbeddedMatter.new (Markdown a.text)
                |> apOrId (marshalColor >> EmbeddedMatter.color) a.color
                |> apOrId (Plain >> EmbeddedMatter.pretext) a.pretext
                |> apOrId (marshalTitle >> EmbeddedMatter.title) a.title
                |> apOrId (marshalAuthor >> EmbeddedMatter.author) a.author
                |> apOrId (marshalImageUrl (Maybe.andThen .link a.title) >> EmbeddedMatter.thumbnail) a.thumbUrl
                |> EmbeddedMatter.attachedFiles
                    (List.filterMap identity [ Maybe.map (marshalImageUrl Nothing >> VisualFile) a.imageUrl ])

        marshalFile f =
            let
                base ctor =
                    case f.thumb360 of
                        Just ( url, width, height ) ->
                            ctor (Url.toString url)
                                |> attachedFileLink (Url.toString f.url_)
                                |> attachedFileDimension (dimension width height)

                        Nothing ->
                            ctor (Url.toString f.url_)
            in
            if Data.Item.mimeIsImage f.mimetype then
                attachedFileDescription f.name (base attachedImage)

            else if Data.Item.mimeIsVideo f.mimetype then
                attachedFileDescription f.name (base attachedVideo)

            else if f.mode == PSlack.Snippet || f.mode == PSlack.Post then
                attachedOther (ExternalLink (Url.toString f.url_))
                    |> attachedFileDescription f.name
                    |> apOrId attachedFilePreview f.preview

            else
                attachedOther (DownloadUrl (Url.toString f.url_))
                    |> attachedFileDescription f.name
    in
    ColumnItem.new id author (Markdown m.text)
        |> ColumnItem.timestamp (PSlack.getPosix m)
        |> ColumnItem.embeddedMatters (List.map marshalAttachment m.attachments)
        |> ColumnItem.attachedFiles (List.map marshalFile m.files)


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
