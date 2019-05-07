module View.Pages.Main exposing (render)

import Array exposing (Array)
import ArrayExtra
import AssocList as Dict
import Broker
import Data.Column as Column
import Data.ColumnStore as ColumnStore
import Data.Filter as Filter exposing (Filter)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial, findDiscordChannel, findSlackConvoCache)
import Data.Item
import Data.Model exposing (Model)
import Data.Msg exposing (..)
import Data.Pref as Pref
import Data.Producer.Discord as PDiscord
import Data.Producer.Discord.Channel as DiscordChannel
import Data.Producer.Discord.Guild as DiscordGuild
import Data.Producer.Discord.Message as DiscordMessage
import Data.Producer.Discord.User as DiscordUser
import Data.Producer.FetchStatus as FetchStatus
import Data.Producer.Slack as PSlack
import Data.Producer.Slack.Bot as SlackBot
import Data.Producer.Slack.Convo as SlackConvo
import Data.Producer.Slack.Message as SlackMessage
import Data.Producer.Slack.Team as SlackTeam
import Data.Producer.Slack.Ts as SlackTs
import Data.Producer.Slack.User as SlackUser
import Data.ProducerRegistry as ProducerRegistry
import Html exposing (Html)
import Id
import List.Extra
import Scroll
import TimeExtra exposing (ms)
import Url
import View.Molecules.Source as Source exposing (Source)
import View.Organisms.Column.Config
import View.Organisms.Column.Header
import View.Organisms.Column.Items
import View.Organisms.Column.Items.ItemForView as ItemForView exposing (ItemForView)
import View.Organisms.Column.Items.ItemForView.Contents exposing (..)
import View.Organisms.Column.Items.ItemForView.EmbeddedMatter as EmbeddedMatter
import View.Organisms.Column.Items.ItemForView.NamedEntity as NamedEntity
import View.Organisms.Column.NewMessageEditor
import View.Organisms.Config.Discord as VDiscord
import View.Organisms.Config.Pref
import View.Organisms.Config.Slack as VSlack
import View.Organisms.Config.Status
import View.Organisms.Modeless as Modeless
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
            , onColumnBorderFlashEnd = \cId -> ColumnCtrl (ColumnStore.ById cId Column.Calm)
            , columnItemsScrollAttrs =
                \c ->
                    Scroll.scrollAttrs (ColumnCtrl << ColumnStore.ById c.id << Column.ScrollMsg) c.items
            , sidebarEffects =
                { onConfigToggleClick = ToggleConfig
                , onAddColumnClick = ColumnCtrl (ColumnStore.AddEmpty m.env.clientHeight)
                , onColumnButtonClickByIndex = ColumnCtrl << ColumnStore.Reveal
                }
            , modelessEffects =
                { onCloseButtonClick = ModelessRemove
                , onAnywhereClick = ModelessTouch
                , onDrag = ModelessMove
                , onDragEnd = ModelessTouch
                }
            }

        props =
            let
                marshalVisibleColumn fam _ c =
                    let
                        ( sources, filters ) =
                            marshalSourcesAndFilters fam (Column.getPendingFilters c)

                        dragStatus =
                            case m.viewState.columnSwapMaybe of
                                Just swap ->
                                    if swap.grabbedId == Column.getId c then
                                        Grabbed

                                    else if swap.pinned == Column.getPinned c then
                                        Droppable

                                    else
                                        Undroppable

                                Nothing ->
                                    Settled
                    in
                    { id = Column.getId c
                    , pinned = Column.getPinned c
                    , sources = sources
                    , filters = filters
                    , dragStatus = dragStatus
                    , configOpen = Column.getConfigOpen c
                    , scrolled = Scroll.scrolled (Column.getItems c)
                    , numItems = Scroll.size (Column.getItems c)
                    , items = Column.getItems c
                    , recentlyTouched = Column.getRecentlyTouched c
                    , editors = Column.getEditors c
                    , editorSeq = Column.getEditorSeq c
                    , userActionOnEditor = Column.getUserActionOnEditor c
                    }

                resolveModelessId mId =
                    case mId of
                        Modeless.RawColumnItemId cId itemIndex ->
                            Dict.get cId m.columnStore.dict
                                |> Maybe.andThen (\c -> Scroll.getAt itemIndex (Column.getItems c))
                                |> Maybe.withDefault Column.itemNotFound
                                |> Modeless.RawColumnItem mId
            in
            { configOpen = m.viewState.configOpen
            , visibleColumns = ColumnStore.mapForView marshalVisibleColumn m.columnStore
            , modeless = Modeless.map resolveModelessId m.viewState.modeless
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
                        , onHeaderClickWhenScrolled = \cId -> ColumnCtrl (ColumnStore.ById cId (Column.ScrollMsg Scroll.BackToTop))
                        , onPinButtonClick = \cId -> ColumnCtrl << ColumnStore.ById cId << Column.Pin
                        , onConfigToggleButtonClick = \cId -> ColumnCtrl << ColumnStore.ById cId << Column.ToggleConfig
                        , onDismissButtonClick = ColumnCtrl << ColumnStore.Dismiss
                        }
                , config =
                    \index c ->
                        View.Organisms.Column.Config.render
                            { onCloseButtonClick = \cId -> ColumnCtrl (ColumnStore.ById cId (Column.ToggleConfig False))
                            , onColumnDeleteButtonClick = ColumnCtrl << ColumnStore.Delete
                            , onSourceSelect =
                                \cId source ->
                                    -- TODO Use brandnew & precise source update Msg
                                    -- Currently it is not working properly!!
                                    -- case source of
                                    --     Source.DiscordSource opts ->
                                    --         ColumnCtrl (ColumnStore.ById cId) (Column.AddFilterAtom { filterIndex = 0, atom = Filter.OfDiscordChannel opts.id })
                                    --
                                    --     Source.SlackSource opts ->
                                    --         ColumnCtrl (ColumnStore.ById cId) (Column.AddFilterAtom { filterIndex = 0, atom = Filter.OfSlackConversation opts.id })
                                    NoOp
                            , selectMsgTagger = SelectCtrl
                            , onRemoveSourceButtonClick =
                                \cId atomIndex ->
                                    -- TODO Use brandnew & precise source update Msg
                                    -- ColumnCtrl (ColumnStore.ById cId) (Column.DelFilterAtom { filterIndex = 0, atomIndex = atomIndex })
                                    NoOp
                            }
                            { selectState = m.viewState.selectState
                            , availableSourecs = List.filter (\s -> not (List.member s c.sources)) availableSources
                            , column = c
                            }
                , newMessageEditor =
                    \c ->
                        View.Organisms.Column.NewMessageEditor.render
                            { onEditorSelect = \cId -> ColumnCtrl << ColumnStore.ById cId << Column.SelectEditor
                            , selectMsgTagger = SelectCtrl
                            , onTextInput = \cId -> ColumnCtrl << ColumnStore.ById cId << Column.EditorInput
                            , onInteracted = \cId -> ColumnCtrl << ColumnStore.ById cId << Column.EditorInteracted
                            , onResetButtonClick = \cId -> ColumnCtrl (ColumnStore.ById cId Column.EditorReset)
                            , onDiscardFileButtonClick = \cId -> ColumnCtrl (ColumnStore.ById cId Column.EditorFileDiscard)
                            , onRequestFileAreaClick = \cId -> ColumnCtrl (ColumnStore.ById cId (Column.EditorFileRequest [ "*/*" ]))
                            , onFileDrop = \cId -> ColumnCtrl << ColumnStore.ById cId << Column.EditorFileSelected
                            , onSubmit = \cId -> ColumnCtrl (ColumnStore.ById cId Column.EditorSubmit)
                            }
                            { selectState = m.viewState.selectState, column = c }
                , items =
                    \c ->
                        let
                            itemsVisible =
                                Scroll.toList c.items
                        in
                        View.Organisms.Column.Items.render
                            { onLoadMoreClick = ColumnCtrl (ColumnStore.ById c.id (Column.ScrollMsg Scroll.LoadMore))
                            , onItemSourceButtonClick = \cId -> ModelessTouch << Modeless.RawColumnItemId cId
                            , onItemRefreshButtonClick = \cId index -> NoOp -- TODO
                            }
                            { timezone = m.viewState.timezone
                            , columnId = c.id
                            , hasMore = List.length itemsVisible < Scroll.size c.items
                            , itemGroups =
                                -- Do note that items are sorted from "newest to oldest" at the moment it came out from Scrolls (itemsVisible).
                                -- Then we reverse, since we want to group items in "older to newer" order, while gloabally showing "newest to oldest" (reverse again)
                                List.indexedMap marshalColumnItem itemsVisible
                                    |> List.reverse
                                    |> List.Extra.groupWhile shouldGroupColumnItem
                                    |> List.reverse
                            }
                }
            }

        availableSources =
            List.concat
                [ case m.columnStore.fam.ofDiscordChannel of
                    Just ( _, channelCaches ) ->
                        let
                            marshalWithGuild c =
                                Maybe.map
                                    (\g ->
                                        Source.discord (Id.to c.id)
                                            c.name
                                            (DiscordGuild.getName g)
                                            (DiscordGuild.iconUrl (Just Source.desiredIconSize) g)
                                    )
                                    c.guildMaybe
                        in
                        List.filterMap marshalWithGuild channelCaches

                    Nothing ->
                        []
                , case m.columnStore.fam.ofSlackConversation of
                    Just { convos } ->
                        let
                            marshal c =
                                Source.slack (Id.to c.id) c.name (SlackTeam.getName c.team) (teamIcon44 c.team) (SlackConvo.isPrivate c.type_)
                        in
                        List.map marshal convos

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
                            ( Source.discord (Id.to channelId)
                                c.name
                                (DiscordGuild.getName g)
                                (DiscordGuild.iconUrl (Just Source.desiredIconSize) g)
                                :: accSources
                            , accFilters
                            )

                        Nothing ->
                            ( accSources, accFilters )

                Filter.OfSlackConversation convoId ->
                    -- TODO support IM/MPIMs
                    case findSlackConvoCache convoId fam of
                        Just c ->
                            let
                                slackSource =
                                    Source.slack (Id.to convoId) c.name (SlackTeam.getName c.team) (teamIcon44 c.team) (SlackConvo.isPrivate c.type_)
                            in
                            ( slackSource :: accSources, accFilters )

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


marshalColumnItem : Int -> Column.ColumnItem -> ItemForView
marshalColumnItem scrollIndex item =
    case item of
        Column.Product offset (Data.Item.DiscordItem message) ->
            marshalDiscordMessage (Broker.offsetToString offset) scrollIndex message

        Column.Product offset (Data.Item.SlackItem message) ->
            marshalSlackMessage (Broker.offsetToString offset) scrollIndex message

        Column.SystemMessage sm ->
            let
                marshalMedia media =
                    case media of
                        Column.Image url ->
                            ItemForView.attachedFiles [ attachedImage (Url.toString url) ]

                        Column.Video url ->
                            ItemForView.attachedFiles [ attachedVideo (Url.toString url) ]
            in
            ItemForView.new sm.id scrollIndex (NamedEntity.new "System Message") (Markdown sm.message)
                |> apOrId marshalMedia sm.mediaMaybe

        Column.LocalMessage lm ->
            ItemForView.new lm.id scrollIndex (NamedEntity.new "Memo") (Markdown lm.message)


apOrId : (a -> b -> b) -> Maybe a -> b -> b
apOrId toFunc =
    Maybe.withDefault identity << Maybe.map toFunc


shouldGroupColumnItem : ItemForView -> ItemForView -> Bool
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


marshalDiscordMessage : String -> Int -> DiscordMessage.Message -> ItemForView
marshalDiscordMessage id scrollIndex m =
    -- XXX Possibly, m.id can be used for interaction handler
    let
        author =
            let
                authorImpl isBot u =
                    let
                        username =
                            DiscordUser.getUsername u
                    in
                    NamedEntity.new username
                        |> NamedEntity.secondaryName ("#" ++ DiscordUser.getDiscriminator u)
                        |> NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (avatarSrc u)) username isBot)

                avatarSrc u =
                    DiscordUser.avatarUrl (Just NamedEntity.desiredIconSize) u
            in
            case DiscordMessage.getAuthor m of
                DiscordMessage.UserAuthor u ->
                    authorImpl False u

                DiscordMessage.WebhookAuthor u ->
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
                |> apOrId EmbeddedMatter.color e.color
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
    ItemForView.new id scrollIndex author (Markdown (DiscordMessage.getContent m))
        |> ItemForView.timestamp (DiscordMessage.getTimestamp m)
        |> ItemForView.attachedFiles (List.map marshalAttachment (DiscordMessage.getAttachments m))
        |> ItemForView.embeddedMatters (List.map marshalEmbed (DiscordMessage.getEmbeds m))


dimension : Int -> Int -> { width : Int, height : Int }
dimension w h =
    { width = w, height = h }


marshalSlackMessage : String -> Int -> SlackMessage.Message -> ItemForView
marshalSlackMessage id scrollIndex m =
    let
        author =
            let
                username =
                    SlackMessage.getAuthorName m

                new =
                    NamedEntity.new username
            in
            case SlackMessage.getAuthor m of
                SlackMessage.UserAuthor u ->
                    let
                        profile =
                            SlackUser.getProfile u
                    in
                    NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Url.toString profile.image48)) username False) new

                SlackMessage.UserAuthorId _ ->
                    new

                SlackMessage.BotAuthor b ->
                    let
                        icons =
                            SlackBot.getIcons b
                    in
                    NamedEntity.avatar (NamedEntity.imageOrAbbr (Just (Url.toString icons.image48)) username True) new

                SlackMessage.BotAuthorId _ ->
                    NamedEntity.avatar (NamedEntity.imageOrAbbr Nothing username True) new

        marshalAttachment a =
            let
                textOrFallback =
                    if String.isEmpty a.text then
                        Markdown a.fallback

                    else
                        Markdown a.text

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
            EmbeddedMatter.new textOrFallback
                |> apOrId EmbeddedMatter.color a.color
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

            else if f.mode == SlackMessage.Snippet || f.mode == SlackMessage.Post then
                attachedOther (ExternalLink (Url.toString f.url_))
                    |> attachedFileDescription f.name
                    |> apOrId attachedFilePreview f.preview

            else
                attachedOther (DownloadUrl (Url.toString f.url_))
                    |> attachedFileDescription f.name
    in
    ItemForView.new id scrollIndex author (Markdown (SlackMessage.getText m))
        |> ItemForView.timestamp (SlackTs.toPosix (SlackMessage.getTs m))
        |> ItemForView.embeddedMatters (List.map marshalAttachment (SlackMessage.getAttachments m))
        |> ItemForView.attachedFiles (List.map marshalFile (SlackMessage.getFiles m))


renderConfigPref : Model -> Html Msg
renderConfigPref m =
    let
        marshalShadowColumn c =
            let
                ( sources, filters ) =
                    marshalSourcesAndFilters m.columnStore.fam (Column.getFilters c)
            in
            { id = Column.getId c, pinned = Column.getPinned c, sources = sources, filters = filters }
    in
    View.Organisms.Config.Pref.render
        { onZephyrModeChange = PrefCtrl << Pref.ZephyrMode
        , onShowColumnButtonClick = \cId -> ColumnCtrl (ColumnStore.ById cId Column.Show)
        , onDeleteColumnButtonClick = ColumnCtrl << ColumnStore.Delete
        }
        { zephyrMode = m.pref.zephyrMode
        , evictThreshold = m.pref.evictThreshold
        , columnSlotsAvailable = not m.pref.zephyrMode || ColumnStore.sizePinned m.columnStore < m.pref.evictThreshold
        , shadowColumns = List.map marshalShadowColumn (ColumnStore.listShadow m.columnStore)
        }


teamIcon44 : SlackTeam.Team -> Maybe String
teamIcon44 t =
    let
        teamIcon =
            SlackTeam.getIcon t
    in
    if teamIcon.imageDefault then
        Nothing

    else
        Just (Url.toString teamIcon.image44)


renderConfigSlack : Model -> Html Msg
renderConfigSlack m =
    let
        effects =
            { onTokenInput = msgTagger << PSlack.UTokenInput
            , onTokenSubmit = msgTagger PSlack.UTokenCommit
            , onRehydrateButtonClick = msgTagger << PSlack.IRehydrate
            , onConvoSelect = \teamId convoId -> msgTagger (PSlack.ISubscribe teamId convoId)
            , onForceFetchButtonClick = \_ _ -> NoOp -- TODO
            , onCreateColumnButtonClick = ColumnCtrl << ColumnStore.AddSimple m.env.clientHeight << Filter.OfSlackConversation
            , onUnsubscribeButtonClick = \teamId convoId -> msgTagger (PSlack.IUnsubscribe teamId convoId)
            , selectMsgTagger = SelectCtrl
            }

        msgTagger =
            ProducerCtrl << ProducerRegistry.SlackMsg

        teamStates =
            let
                marshal ( teamId, teamState ) =
                    let
                        marshalTeam t =
                            VSlack.TeamSnip teamId (SlackTeam.getName t) (SlackTeam.getDomain t) (teamIcon44 t)

                        marshalUser u =
                            let
                                profile =
                                    SlackUser.getProfile u
                            in
                            VSlack.UserSnip profile.realName profile.displayName (Url.toString profile.image48)

                        hydratedOnce rehydrating pov =
                            let
                                ( subbable, subbed ) =
                                    -- TODO support IM/MPIM
                                    Dict.values pov.convos
                                        |> List.filter (\c -> not (SlackConvo.getIsArchived c) && SlackConvo.isChannel (SlackConvo.getType_ c))
                                        |> List.sortWith SlackConvo.compare
                                        |> List.foldr partitionThenMarshal ( [], [] )

                                partitionThenMarshal c ( accNotSubbed, accSubbed ) =
                                    if FetchStatus.dormant (SlackConvo.getFetchStatus c) then
                                        ( VSlack.SubbableConvo (SlackConvo.getId c)
                                            (SlackConvo.getName c)
                                            (SlackConvo.isPrivate (SlackConvo.getType_ c))
                                            :: accNotSubbed
                                        , accSubbed
                                        )

                                    else
                                        let
                                            marshalled =
                                                VSlack.SubbedConvo (SlackConvo.getId c)
                                                    (SlackConvo.getName c)
                                                    (SlackConvo.isPrivate (SlackConvo.getType_ c))
                                                    (FetchStatus.fetching (SlackConvo.getFetchStatus c))
                                                    (FetchStatus.subscribed (SlackConvo.getFetchStatus c))
                                        in
                                        ( accNotSubbed, marshalled :: accSubbed )
                            in
                            VSlack.hydratedOnce rehydrating (marshalUser pov.user) subbable subbed
                    in
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
            in
            -- Sort is necessary since Dict is AssocList
            m.producerRegistry.slack.dict
                |> Dict.toList
                |> List.sortBy (Tuple.first >> Id.to)
                |> List.map marshal
    in
    VSlack.render effects <|
        case m.producerRegistry.slack.unidentified of
            PSlack.TokenWritable token ->
                VSlack.Props (PSlack.fromToken token) True teamStates m.viewState.selectState

            PSlack.TokenIdentifying token ->
                VSlack.Props (PSlack.fromToken token) False teamStates m.viewState.selectState


renderConfigDiscord : Model -> Html Msg
renderConfigDiscord m =
    let
        effects =
            { onTokenInput = msgTagger << PDiscord.TokenInput
            , onTokenSubmit = msgTagger PDiscord.TokenCommit
            , onRehydrateButtonClick = msgTagger PDiscord.Rehydrate
            , onChannelSelect = msgTagger << PDiscord.Subscribe
            , onForceFetchButtonClick = always NoOp -- TODO
            , onCreateColumnButtonClick = ColumnCtrl << ColumnStore.AddSimple m.env.clientHeight << Filter.OfDiscordChannel
            , onUnsubscribeButtonClick = msgTagger << PDiscord.Unsubscribe
            , selectMsgTagger = SelectCtrl
            }

        msgTagger =
            ProducerCtrl << ProducerRegistry.DiscordMsg

        hydratedOnce rehydrating pov =
            let
                ( subbable, subbed ) =
                    Dict.values pov.channels
                        |> List.sortWith DiscordChannel.compare
                        |> List.foldr partitionThenMarshal ( [], [] )

                partitionThenMarshal c ( accNotSubbed, accSubbed ) =
                    if FetchStatus.dormant (DiscordChannel.getFetchStatus c) then
                        let
                            marshalled =
                                VDiscord.SubbableChannel (DiscordChannel.getId c)
                                    (DiscordChannel.getName c)
                                    (DiscordChannel.getGuildMaybe c)
                        in
                        ( marshalled :: accNotSubbed, accSubbed )

                    else
                        let
                            marshalled =
                                VDiscord.SubbedChannel (DiscordChannel.getId c)
                                    (DiscordChannel.getName c)
                                    (DiscordChannel.getGuildMaybe c)
                                    (FetchStatus.fetching (DiscordChannel.getFetchStatus c))
                                    (FetchStatus.subscribed (DiscordChannel.getFetchStatus c))
                        in
                        ( accNotSubbed, marshalled :: accSubbed )
            in
            VDiscord.hydratedOnce rehydrating pov.user pov.guilds subbable subbed
    in
    VDiscord.render effects <|
        case m.producerRegistry.discord of
            PDiscord.TokenWritable token ->
                let
                    rawToken =
                        PDiscord.fromToken token
                in
                VDiscord.Props rawToken "Register" (not (String.isEmpty rawToken)) VDiscord.NotIdentified m.viewState.selectState

            PDiscord.TokenReady token ->
                VDiscord.Props (PDiscord.fromToken token) "Waiting..." False VDiscord.NotIdentified m.viewState.selectState

            PDiscord.Identified s ->
                VDiscord.Props (PDiscord.fromToken s.token) "Fetching data..." False (VDiscord.NowHydrating s.user) m.viewState.selectState

            PDiscord.Hydrated token pov ->
                let
                    rawToken =
                        PDiscord.fromToken token

                    text =
                        if String.isEmpty rawToken then
                            "Unregister"

                        else
                            "Change Token"
                in
                VDiscord.Props rawToken text (token /= pov.token) (hydratedOnce False pov) m.viewState.selectState

            PDiscord.Rehydrating token pov ->
                VDiscord.Props (PDiscord.fromToken token) "Fetching data..." False (hydratedOnce True pov) m.viewState.selectState

            PDiscord.Revisit pov ->
                VDiscord.Props (PDiscord.fromToken pov.token) "Reloading..." False (hydratedOnce True pov) m.viewState.selectState

            PDiscord.Expired token pov ->
                let
                    text =
                        if token == pov.token then
                            "Expired"

                        else
                            "Change Token"
                in
                VDiscord.Props (PDiscord.fromToken token) text (token /= pov.token) (hydratedOnce False pov) m.viewState.selectState

            PDiscord.Switching s pov ->
                VDiscord.Props (PDiscord.fromToken s.token) "Switching user..." False (hydratedOnce False pov) m.viewState.selectState


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
