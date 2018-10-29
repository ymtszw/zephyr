module View exposing (body)

import Array exposing (Array)
import Broker exposing (Offset)
import Data.ColorTheme exposing (brightness, oneDark)
import Data.Column as Column exposing (ColumnItem(..), Media(..))
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter as Filter exposing (Filter(..), FilterAtom(..), MediaFilter(..))
import Data.Item exposing (Item(..))
import Data.Model exposing (ColumnSwap, Model, ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Data.TextRenderer exposing (TextRenderer)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Region exposing (description)
import Html
import Html.Attributes exposing (draggable, style, title)
import Html.Events
import Iso8601
import Json.Decode as D exposing (Decoder)
import ListExtra as List
import Logger
import Octicons
import String exposing (fromFloat, fromInt)
import TimeExtra as Time exposing (ms)
import Url
import View.Parts exposing (noneAttr, octiconEl, octiconFreeSizeEl, scale12, squareIconEl)
import View.Select as Select exposing (select)


body : Model -> List (Html.Html Msg)
body m =
    [ layout [ dragEventHandlers m.viewState.columnSwapMaybe ] (bodyEl m)
    , fancyScroll
    ]


bodyEl : Model -> Element Msg
bodyEl model =
    backgroundEl <|
        row [ width fill, height fill, clipY ]
            [ sidebarEl model
            , if model.viewState.configOpen then
                configPaneEl model

              else
                none
            , columnsEl model
            ]


backgroundEl : Element Msg -> Element Msg
backgroundEl contents =
    row
        [ BG.color oneDark.bg
        , width fill
        , height fill
        , inFront contents
        ]
        [ el
            [ centerY
            , centerX
            , Font.bold
            , Font.color oneDark.sub
            , Font.size (scale12 12)
            , Font.center
            , Font.family [ Font.serif ]
            ]
            (text "Zephyr")
        ]


dragEventHandlers : Maybe ColumnSwap -> Attribute Msg
dragEventHandlers columnSwapMaybe =
    case columnSwapMaybe of
        Just _ ->
            htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))

        Nothing ->
            noneAttr



-- SIDEBAR


sidebarEl : Model -> Element Msg
sidebarEl { columnStore, viewState, env } =
    column
        [ width (px 50)
        , height (fill |> maximum env.clientHeight)
        , paddingXY 0 10
        , BG.color oneDark.bg
        ]
        [ el [ width fill, alignTop ] (columnButtonsEl columnStore)
        , el [ width fill, alignBottom ] (otherButtonsEl viewState)
        ]


columnButtonsEl : ColumnStore -> Element Msg
columnButtonsEl columnStore =
    List.append (ColumnStore.indexedMap columnButtonEl columnStore) [ ( "columnAddButton", columnAddButtonEl ) ]
        |> Element.Keyed.column [ width fill, padding 5, spacingXY 0 10 ]


columnButtonEl : Int -> Column.Column -> ( String, Element Msg )
columnButtonEl index { id } =
    ( "sidebarButton_" ++ id
    , el [ width fill ] <|
        Element.Input.button
            [ width (px 40)
            , height (px 40)
            , clip
            , Font.color oneDark.note
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just (DelColumn index), label = el [ centerX, centerY ] <| text "×" }
    )


columnAddButtonEl : Element Msg
columnAddButtonEl =
    el [ width fill ] <|
        Element.Input.button
            [ width (px 40)
            , height (px 40)
            , clip
            , Font.color oneDark.note
            , BD.dashed
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just AddColumn, label = el [ centerX, centerY ] <| text "+" }


otherButtonsEl : ViewState -> Element Msg
otherButtonsEl viewState =
    column [ width fill, padding 5, spacingXY 0 10 ]
        [ Element.Input.button
            [ width (px 40)
            , height (px 40)
            , BD.rounded 10
            , if viewState.configOpen then
                BG.color oneDark.main

              else
                mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not viewState.configOpen))
            , label = el [ centerX, centerY ] <| octiconEl Octicons.gear
            }
        , newTabLink
            [ width (px 40)
            , height (px 40)
            , BD.rounded 10
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = el [ centerX, centerY ] <| octiconEl Octicons.markGithub
            }
        ]



-- COLUMNS


columnsEl : Model -> Element Msg
columnsEl m =
    Element.Keyed.row
        [ width fill
        , height (fill |> maximum m.env.clientHeight)
        , Font.regular
        ]
        (ColumnStore.indexedMap (columnKeyEl m) m.columnStore)


columnKeyEl : Model -> Int -> Column.Column -> ( String, Element Msg )
columnKeyEl m index column =
    Tuple.pair ("column_" ++ column.id) <|
        case m.viewState.columnSwapMaybe of
            Nothing ->
                notDraggedColumnEl m index column <|
                    if m.viewState.columnSwappable then
                        [ htmlAttribute (draggable "true")
                        , htmlAttribute (style "cursor" "all-scroll")
                        , htmlAttribute (Html.Events.on "dragstart" (onDragStart index column.id))
                        ]

                    else
                        []

            Just swap ->
                if swap.grabbedId == column.id then
                    draggedColumnEl m.env.clientHeight

                else
                    notDraggedColumnEl m index column <|
                        [ htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter index, True ))) ]


notDraggedColumnEl : Model -> Int -> Column.Column -> List (Attribute Msg) -> Element Msg
notDraggedColumnEl m index c attrs =
    column
        (columnBaseAttrs m.env.clientHeight ++ attrs)
        [ columnHeaderEl c
        , columnConfigEl m index c
        , case c.items of
            [] ->
                waitingForFirstItemEl

            items ->
                -- Do note that items are sorted from latest to oldest
                items
                    |> List.groupWhile shouldGroup
                    |> List.map (itemEl m)
                    |> column [ width fill, paddingXY 5 0, scrollbarY ]
        ]


waitingForFirstItemEl : Element Msg
waitingForFirstItemEl =
    el [ width fill, height (px 50), paddingXY 5 0 ] <|
        el [ centerX, centerY, Font.color oneDark.note, Font.size (scale12 2) ] <|
            text "Waiting for messages..."


shouldGroup : ColumnItem -> ColumnItem -> Bool
shouldGroup newer older =
    case ( newer, older ) of
        ( System _, _ ) ->
            False

        ( _, System _ ) ->
            False

        ( Product _ (DiscordItem dNewer), Product _ (DiscordItem dOlder) ) ->
            shouldGroupDiscordMessage dNewer dOlder


shouldGroupDiscordMessage : Discord.Message -> Discord.Message -> Bool
shouldGroupDiscordMessage dNewer dOlder =
    (dNewer.channelId == dOlder.channelId)
        && (dNewer.author == dOlder.author)
        && (ms dOlder.timestamp + 60000 > ms dNewer.timestamp)


draggedColumnEl : Int -> Element Msg
draggedColumnEl clientHeight =
    el (columnBaseAttrs clientHeight ++ [ BG.color oneDark.bg ]) none


columnBaseAttrs : Int -> List (Attribute Msg)
columnBaseAttrs clientHeight =
    [ width (px fixedColumnWidth)
    , height (fill |> maximum clientHeight)
    , BG.color oneDark.main
    , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
    , BD.color oneDark.bg
    , Font.color oneDark.text
    ]


fixedColumnWidth : Int
fixedColumnWidth =
    350


onDragStart : Int -> String -> Decoder Msg
onDragStart index id =
    let
        fireDependingOnDataTransfer types =
            case types of
                [] ->
                    -- If Column div elements are dragged, it should not have items attached in dataTransfer property
                    D.succeed (DragStart index id)

                _ ->
                    -- Otherwise something else (img, link, etc...) are dragged. Turn off swap mode
                    D.succeed (ToggleColumnSwappable False)
    in
    D.at [ "dataTransfer", "types" ] (D.list D.string)
        |> D.andThen fireDependingOnDataTransfer


columnHeaderEl : Column.Column -> Element Msg
columnHeaderEl column =
    row
        [ width fill
        , padding 10
        , BG.color oneDark.sub
        ]
        [ text ("[PH] " ++ column.id)
        , Element.Input.button [ alignRight ]
            { onPress = Just (ToggleColumnConfig column.id (not column.configOpen))
            , label = octiconEl Octicons.settings
            }
        ]


columnConfigEl : Model -> Int -> Column.Column -> Element Msg
columnConfigEl m index c =
    if c.configOpen then
        column
            [ width fill
            , padding 5
            , spacing 3
            , BG.color oneDark.sub
            , BD.width 1
            , BD.color oneDark.note
            ]
            [ columnConfigTitleEl "Filter Rules"
            , filtersEl m c
            , columnConfigTitleEl "Danger Zone"
            , columnDeleteEl index c
            , Element.Input.button [ width fill, BG.color oneDark.sub ]
                { onPress = Just (ToggleColumnConfig c.id False)
                , label = octiconFreeSizeEl 24 Octicons.triangleUp
                }
            ]

    else
        none


columnConfigTitleEl : String -> Element Msg
columnConfigTitleEl title =
    el
        [ width fill
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size (scale12 3)
        , Font.color oneDark.note
        ]
        (text title)


filtersEl : Model -> Column.Column -> Element Msg
filtersEl m c =
    Array.indexedMap (filterEl m c.id) c.filters
        |> Array.push (addNewFilterEl m c.id)
        |> Array.toList
        |> List.intersperse (filterLogicSeparator "AND")
        |> column
            [ width (fill |> minimum 0)
            , padding 5
            , spacing 3
            , BD.rounded 5
            , BG.color oneDark.main
            ]


filterEl : Model -> String -> Int -> Filter -> Element Msg
filterEl m cId index filter =
    let
        tagger newFilterMaybe =
            case newFilterMaybe of
                Just newFilter ->
                    SetColumnFilter cId index newFilter

                Nothing ->
                    DelColumnFilter cId index
    in
    filterGeneratorEl tagger m cId (Just ( index, filter ))


addNewFilterEl : Model -> String -> Element Msg
addNewFilterEl m cId =
    let
        tagger newFilterMaybe =
            case newFilterMaybe of
                Just newFilter ->
                    AddColumnFilter cId newFilter

                Nothing ->
                    -- Should not happen
                    tagger newFilterMaybe
    in
    filterGeneratorEl tagger m cId Nothing


filterLogicSeparator : String -> Element msg
filterLogicSeparator operator =
    el
        [ width (fill |> minimum 0)
        , padding 3
        , Font.size (scale12 2)
        , Font.color oneDark.note
        ]
        (el [ centerX ] (text operator))


filterGeneratorEl : (Maybe Filter -> Msg) -> Model -> String -> Maybe ( Int, Filter ) -> Element Msg
filterGeneratorEl tagger m cId indexFilterMaybe =
    row
        [ width fill
        , BD.width 1
        , BD.rounded 5
        , BD.color oneDark.note
        ]
        [ case indexFilterMaybe of
            Just ( index, filter ) ->
                let
                    filterId =
                        cId ++ "-filter_" ++ String.fromInt index
                in
                column [ width (fill |> minimum 0), padding 5 ] <|
                    List.intersperse (filterLogicSeparator "OR") <|
                        Filter.indexedMap (filterAtomEl filter tagger m filterId) filter
                            ++ [ newFilterAtomEl (\fa -> tagger (Just (Filter.append fa filter))) m filterId ]

            Nothing ->
                column [ width (fill |> minimum 0), padding 5 ]
                    [ newFilterAtomEl (tagger << Just << Singular) m (cId ++ "addNewFilter") ]
        , deleteFilterButtonEl cId indexFilterMaybe
        ]


deleteFilterButtonEl : String -> Maybe ( Int, Filter ) -> Element Msg
deleteFilterButtonEl cId indexFilterMaybe =
    case indexFilterMaybe of
        Just ( index, _ ) ->
            Element.Input.button
                [ width (px 20)
                , height fill
                , mouseOver [ BG.color oneDark.err ]
                , focused [ BG.color oneDark.err ]
                , alignRight
                , BD.roundEach { topLeft = 0, topRight = 5, bottomRight = 5, bottomLeft = 0 }
                ]
                { onPress = Just (DelColumnFilter cId index)
                , label = el [ centerY, centerX ] <| octiconFreeSizeEl 16 Octicons.trashcan
                }

        Nothing ->
            none


filterAtomEl : Filter -> (Maybe Filter -> Msg) -> Model -> String -> Int -> FilterAtom -> Element Msg
filterAtomEl originalFilter tagger m filterId index filterAtom =
    let
        updateAndTag newFilterAtom =
            tagger <|
                case newFilterAtom of
                    RemoveMe ->
                        Filter.removeAt index originalFilter

                    _ ->
                        Just (Filter.setAt index newFilterAtom originalFilter)
    in
    filterAtomInputEl updateAndTag m (filterId ++ "-atom_" ++ String.fromInt index) (Just filterAtom)


newFilterAtomEl : (FilterAtom -> Msg) -> Model -> String -> Element Msg
newFilterAtomEl tagger m filterId =
    filterAtomInputEl tagger m (filterId ++ "newAtom") Nothing


filterAtomInputEl : (FilterAtom -> Msg) -> Model -> String -> Maybe FilterAtom -> Element Msg
filterAtomInputEl tagger m filterAtomId filterAtomMaybe =
    let
        material =
            filterAtomMaterial m.producerRegistry
    in
    row [ width (fill |> minimum 0), spacing 3 ]
        [ filterAtomTypeSelectEl tagger m.viewState.selectState material (filterAtomId ++ "-typeSelect") filterAtomMaybe
        , filterAtomVariableInputEl tagger m.viewState.selectState material (filterAtomId ++ "-variableInput") filterAtomMaybe
        ]


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe ( FilterAtom, Dict String Discord.Channel )
    }


filterAtomMaterial : ProducerRegistry -> FilterAtomMaterial
filterAtomMaterial producerRegistry =
    { ofDiscordChannel =
        case producerRegistry.discord |> Maybe.andThen Discord.getPov of
            Just { channels } ->
                let
                    filtered =
                        Dict.filter (\_ c -> FetchStatus.isAvailable c.fetchStatus) channels
                in
                case Dict.values filtered of
                    [] ->
                        Nothing

                    c :: _ ->
                        Just ( OfDiscordChannel c.id, filtered )

            Nothing ->
                Nothing
    }


filterAtomTypeSelectEl : (FilterAtom -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomTypeSelectEl tagger selectState material selectId filterAtomMaybe =
    el [ width (fill |> maximum 120) ] <|
        select
            { id = selectId
            , onSelect = tagger
            , selectedOption = filterAtomMaybe
            , noMsgOptionEl = filterAtomTypeOptionEl
            }
            selectState
            (availableFilterAtomsWithDefaultArguments material filterAtomMaybe)


filterAtomTypeOptionEl : FilterAtom -> Element msg
filterAtomTypeOptionEl filterAtom =
    el [ Font.size (scale12 1) ] <|
        case filterAtom of
            ByMessage _ ->
                text "Message contains..."

            ByMedia _ ->
                text "Attached media..."

            OfDiscordChannel _ ->
                text "Discord message in channel..."

            RemoveMe ->
                text "Remove this filter"


availableFilterAtomsWithDefaultArguments : FilterAtomMaterial -> Maybe FilterAtom -> List FilterAtom
availableFilterAtomsWithDefaultArguments material filterAtomMaybe =
    replaceWithSelected filterAtomMaybe
        (basicFilterAtoms ++ materialToDefaultFilterAtoms material)
        ++ Maybe.withDefault [] (Maybe.map (always [ RemoveMe ]) filterAtomMaybe)


basicFilterAtoms : List FilterAtom
basicFilterAtoms =
    [ ByMessage "text"
    , ByMedia HasNone
    ]


materialToDefaultFilterAtoms : FilterAtomMaterial -> List FilterAtom
materialToDefaultFilterAtoms material =
    List.filterMap identity
        [ Maybe.map Tuple.first material.ofDiscordChannel
        ]


replaceWithSelected : Maybe FilterAtom -> List FilterAtom -> List FilterAtom
replaceWithSelected filterAtomMaybe filterAtoms =
    case filterAtomMaybe of
        Just filterAtom ->
            let
                replaceIfSameType selected option =
                    case ( selected, option ) of
                        ( ByMessage _, ByMessage _ ) ->
                            selected

                        ( ByMedia _, ByMedia _ ) ->
                            selected

                        ( OfDiscordChannel _, OfDiscordChannel _ ) ->
                            selected

                        _ ->
                            option
            in
            List.map (replaceIfSameType filterAtom) filterAtoms

        Nothing ->
            filterAtoms


filterAtomVariableInputEl : (FilterAtom -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomVariableInputEl tagger selectState material inputId filterAtomMaybe =
    case filterAtomMaybe of
        Just (ByMessage query) ->
            filterAtomVariableTextInputEl (tagger << ByMessage) query

        Just (ByMedia mediaType) ->
            filterAtomVariableSelectInputEl (tagger << ByMedia) selectState (inputId ++ "-variableSelect") mediaType <|
                ( [ HasNone, HasImage, HasMovie ], mediaTypeOptionEl )

        Just (OfDiscordChannel cId) ->
            filterAtomVariableSelectInputEl (tagger << OfDiscordChannel) selectState (inputId ++ "-variableSelect") cId <|
                case material.ofDiscordChannel of
                    Just ( _, channels ) ->
                        ( Dict.values channels |> List.sortWith discordChannelSorter |> List.map .id
                        , discordChannelWithGuildIconEl channels
                        )

                    Nothing ->
                        ( [], text )

        Just RemoveMe ->
            -- Should not happen
            none

        Nothing ->
            none


discordChannelSorter : Discord.Channel -> Discord.Channel -> Order
discordChannelSorter a b =
    let
        gName =
            -- Tilde is sorted AFTER "z" in ordinary sort algorithms, suitable for fallback
            .guildMaybe >> Maybe.map .name >> Maybe.withDefault "~~~"
    in
    case compare (gName a) (gName b) of
        EQ ->
            compare a.name b.name

        diff ->
            diff


filterAtomVariableTextInputEl : (String -> Msg) -> String -> Element Msg
filterAtomVariableTextInputEl tagger text =
    Element.Input.text
        [ width fill
        , height (px 30) -- Match with select input height
        , padding 5
        , BG.color oneDark.note
        , BD.width 0
        , Font.size (scale12 1)
        ]
        { onChange = tagger
        , text = text
        , placeholder = Nothing
        , label = Element.Input.labelHidden "text"
        }


filterAtomVariableSelectInputEl : (a -> Msg) -> Select.State -> String -> a -> ( List a, a -> Element Msg ) -> Element Msg
filterAtomVariableSelectInputEl tagger selectState selectId selected ( options, optionEl ) =
    select
        { id = selectId
        , onSelect = tagger
        , selectedOption = Just selected
        , noMsgOptionEl = el [ Font.size (scale12 1) ] << optionEl
        }
        selectState
        options


mediaTypeOptionEl : MediaFilter -> Element msg
mediaTypeOptionEl mediaType =
    case mediaType of
        HasNone ->
            text "None"

        HasImage ->
            text "Image"

        HasMovie ->
            text "Movie"


discordGuildOptionEl : Dict String Discord.Guild -> String -> Element msg
discordGuildOptionEl guilds gId =
    case Dict.get gId guilds of
        Just guild ->
            row [ width fill, spacing 3 ]
                [ Discord.guildSmallIconEl guild, text guild.name ]

        Nothing ->
            text gId


discordChannelWithGuildIconEl : Dict String Discord.Channel -> String -> Element msg
discordChannelWithGuildIconEl channels cId =
    case Dict.get cId channels of
        Just channel ->
            case channel.guildMaybe of
                Just guild ->
                    row [ width (fill |> minimum 0), spacing 3 ]
                        [ Discord.guildSmallIconEl guild
                        , text ("#" ++ channel.name)
                        ]

                Nothing ->
                    -- Mostly DM
                    text channel.name

        Nothing ->
            text cId


columnDeleteEl : Int -> Column.Column -> Element Msg
columnDeleteEl index column =
    row [ width fill, spacing 5, padding 10 ]
        [ columnDeleteGateEl column
        , columnDeleteButtonEl index column
        ]


columnDeleteGateEl : Column.Column -> Element Msg
columnDeleteGateEl column =
    Element.Input.text
        [ width fill
        , height (px 30) -- Match with select input height
        , padding 5
        , BG.color oneDark.note
        , BD.width 0
        , Font.size (scale12 1)
        ]
        { onChange = ColumnDeleteGateInput column.id
        , text = column.deleteGate
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    el [ centerY ] (text "Type DELETE to delete this column")
        , label = Element.Input.labelHidden "Delete Gate"
        }


columnDeleteButtonEl : Int -> Column.Column -> Element Msg
columnDeleteButtonEl index column =
    el [ width (px 100) ] <|
        if String.toLower column.deleteGate == "delete" then
            Element.Input.button
                [ width fill
                , height (px 30)
                , BD.rounded 5
                , BG.color oneDark.err
                ]
                { onPress = Just (DelColumn index)
                , label = el [ centerX ] (text "Delete!")
                }

        else
            none



-- ITEM


itemEl : Model -> List ColumnItem -> Element Msg
itemEl m closeItems =
    -- Reverse, since we want to show closeItems in oldest to latest, opposite from other places
    case List.reverse closeItems of
        [] ->
            -- Should not happen
            none

        item :: items ->
            row
                [ width fill
                , paddingXY 0 5
                , spacing 5
                , BD.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
                , BD.color oneDark.bd
                ]
                [ itemAvatarEl item
                , itemContentsEl m item items
                ]


itemAvatarEl : ColumnItem -> Element Msg
itemAvatarEl item =
    case item of
        Product _ (DiscordItem { author }) ->
            case author of
                Discord.UserAuthor user ->
                    avatarWithBadgeEl
                        { badge = Nothing
                        , fallback = user.username
                        , url = Just <| Discord.imageUrlWithFallback (Just "64") user.discriminator user.avatar
                        }

                Discord.WebhookAuthor user ->
                    avatarWithBadgeEl
                        { badge = Just botIconEl
                        , fallback = user.username
                        , url = Just <| Discord.imageUrlWithFallback (Just "64") user.discriminator user.avatar
                        }

        System _ ->
            avatarWithBadgeEl { badge = Nothing, fallback = "Zephyr", url = Nothing }


avatarWithBadgeEl : { badge : Maybe (Element Msg), fallback : String, url : Maybe String } -> Element Msg
avatarWithBadgeEl { badge, fallback, url } =
    let
        bottomRightBadge =
            case badge of
                Just badgeEl ->
                    [ alignTop, inFront <| el [ alignBottom, alignRight ] <| badgeEl ]

                Nothing ->
                    [ alignTop ]
    in
    el bottomRightBadge <| el [ padding 2 ] <| squareIconEl avatarSize fallback <| url


avatarSize : Int
avatarSize =
    40


botIconEl : Element Msg
botIconEl =
    el [ padding 1, BG.color oneDark.succ, BD.rounded 2, htmlAttribute (title "BOT") ] <|
        octiconFreeSizeEl 12 Octicons.zap


itemContentsEl : Model -> ColumnItem -> List ColumnItem -> Element Msg
itemContentsEl m item closeItems =
    case item of
        Product offset (DiscordItem discordMessage) ->
            let
                unwrap cItem =
                    case cItem of
                        Product o (DiscordItem dm) ->
                            Just ( dm, o )

                        _ ->
                            Nothing
            in
            closeItems
                |> List.filterMap unwrap
                |> discordMessageEl m ( discordMessage, offset )

        System { message, mediaMaybe } ->
            defaultItemEl message mediaMaybe


discordMessageEl : Model -> ( Discord.Message, Offset ) -> List ( Discord.Message, Offset ) -> Element Msg
discordMessageEl m ( discordMessage, _ ) closeMessages =
    -- TODO match with official app styling
    column [ width fill, spacing 5, alignTop ] <|
        (::) (discordMessageHeaderEl m discordMessage) <|
            List.map (discordMessageBodyEl m) <|
                (::) discordMessage <|
                    List.map Tuple.first closeMessages


discordMessageHeaderEl : Model -> Discord.Message -> Element Msg
discordMessageHeaderEl m { author, timestamp, channelId } =
    let
        userNameEl =
            paragraph [ alignLeft, Font.bold, Font.size (scale12 2) ]
                [ text <|
                    case author of
                        Discord.UserAuthor user ->
                            user.username

                        Discord.WebhookAuthor user ->
                            user.username
                ]
    in
    row [ width fill, spacing 5 ]
        [ userNameEl
        , el [ alignRight, Font.color oneDark.note, Font.size (scale12 1) ] <|
            text (Time.local m.viewState.timezone timestamp)
        ]


discordMessageBodyEl : Model -> Discord.Message -> Element Msg
discordMessageBodyEl m discordMessage =
    textColumn [ spacingXY 0 10, width fill ]
        [ messageToParagraph discordMessage.content
        , column [ width fill, spacing 5 ] <| List.map discordEmbedEl discordMessage.embeds
        ]


discordEmbedEl : Discord.Embed -> Element Msg
discordEmbedEl embed =
    [ embed.author |> Maybe.map discordEmbedAuthorEl
    , embed.title |> Maybe.map (discordEmbedTitleEl embed.url)
    , embed.description |> Maybe.map messageToParagraph
    , embed.image |> Maybe.map (discordEmbedImageEl maxEmbeddedMediaWidth embed.url)
    ]
        |> List.filterMap identity
        |> textColumn
            [ width fill
            , spacing 5
            , Font.size (scale12 1)
            , htmlAttribute (style "white-space" "pre-wrap")
            , htmlAttribute (style "word-break" "break-all")
            ]
        |> discordSmartThumbnailEl embed


maxEmbeddedMediaWidth : Int
maxEmbeddedMediaWidth =
    maxMediaWidth - 15


discordEmbedAuthorEl : Discord.EmbedAuthor -> Element Msg
discordEmbedAuthorEl author =
    let
        wrapWithLink element =
            case author.url of
                Just url ->
                    newTabLink [] { url = Url.toString url, label = element }

                Nothing ->
                    element
    in
    row [ spacing 5, Font.bold ]
        [ wrapWithLink <| squareIconEl (avatarSize // 2) author.name <| Maybe.map Url.toString author.proxyIconUrl
        , paragraph [] [ wrapWithLink <| text author.name ]
        ]


discordEmbedTitleEl : Maybe Url.Url -> String -> Element Msg
discordEmbedTitleEl urlMaybe title =
    paragraph [ Font.color oneDark.link ]
        [ case urlMaybe of
            Just url ->
                newTabLink [] { url = Url.toString url, label = text title }

            Nothing ->
                text title
        ]


discordEmbedImageEl : Int -> Maybe Url.Url -> Discord.EmbedImage -> Element Msg
discordEmbedImageEl maxWidth linkUrlMaybe embedImage =
    let
        availableSrc =
            embedImage.proxyUrl |> Maybe.withDefault embedImage.url

        linkedImage actualMaxWidth src =
            newTabLink []
                { url = Url.toString (Maybe.withDefault embedImage.url linkUrlMaybe)
                , label =
                    image [ width (fill |> maximum actualMaxWidth) ]
                        { src = Url.toString src, description = "Thumbnail" }
                }
    in
    case ( embedImage.width, embedImage.height ) of
        ( Just w, Just h ) ->
            let
                ( queryW, queryH ) =
                    if w <= maxWidth then
                        ( w, h )

                    else
                        ( maxWidth, round <| toFloat h * (toFloat maxWidth / toFloat w) )
            in
            linkedImage queryW <|
                { availableSrc | query = Just ("width=" ++ fromInt queryW ++ "&height=" ++ fromInt queryH) }

        _ ->
            linkedImage maxMediaWidth availableSrc


discordSmartThumbnailEl : Discord.Embed -> Element Msg -> Element Msg
discordSmartThumbnailEl embed element =
    let
        wrapperAttrs =
            [ padding 5
            , spacing 5
            , BG.color (brightness -1 oneDark.main)
            , BD.color (Maybe.withDefault oneDark.bg embed.color)
            , BD.widthEach { left = 4, top = 0, right = 0, bottom = 0 }
            , BD.rounded 3
            ]

        linkUrlMaybe =
            case ( embed.video, embed.url ) of
                ( Just ev, _ ) ->
                    Just ev.url

                ( _, eu ) ->
                    eu
    in
    case embed.thumbnail of
        Just embedImage ->
            -- Assuming embed.video always comes with embed.thumbnail
            -- TODO load embedded players on click
            if iconLike embedImage.width embedImage.height then
                row wrapperAttrs
                    [ element
                    , el [ alignTop, alignRight ] <| discordEmbedImageEl maxThumbnailWidth linkUrlMaybe embedImage
                    ]

            else
                column wrapperAttrs
                    [ element
                    , el [ alignLeft ] <| discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe embedImage
                    ]

        Nothing ->
            row wrapperAttrs [ element ]


iconLike : Maybe Int -> Maybe Int -> Bool
iconLike widthMaybe heightMaybe =
    let
        mapper w h =
            w == h || w <= maxThumbnailWidth
    in
    Maybe.map2 mapper widthMaybe heightMaybe
        |> Maybe.withDefault False


maxThumbnailWidth : Int
maxThumbnailWidth =
    60


defaultItemEl : String -> Maybe Media -> Element Msg
defaultItemEl message mediaMaybe =
    case mediaMaybe of
        Just media ->
            textColumn [ spacingXY 0 10, width fill, alignTop ]
                [ messageToParagraph message
                , mediaEl media
                ]

        Nothing ->
            el [ width fill, alignTop ] (messageToParagraph message)


messageToParagraph : String -> Element Msg
messageToParagraph message =
    message
        |> Data.TextRenderer.default oneDark
        |> List.map html
        |> paragraph
            [ Font.size (scale12 1)
            , htmlAttribute (style "white-space" "pre-wrap")
            , htmlAttribute (style "word-break" "break-all")
            ]


mediaEl : Media -> Element Msg
mediaEl media =
    case media of
        Image url ->
            imageEl "Image" url

        Movie url ->
            videoEl url


imageEl : String -> Url.Url -> Element Msg
imageEl desc url =
    image [ width (fill |> maximum maxMediaWidth) ] { src = Url.toString url, description = desc }


maxMediaWidth : Int
maxMediaWidth =
    fixedColumnWidth - avatarSize - 10


videoEl : Url.Url -> Element Msg
videoEl url =
    el [ width fill, centerX ] <|
        html <|
            Html.video
                [ Html.Attributes.controls True
                , Html.Attributes.width maxMediaWidth
                , Html.Attributes.src (Url.toString url)
                ]
                [ Html.text "Embedded video not supported."
                , Html.a [ Html.Attributes.href (Url.toString url) ] [ Html.text "[Source]" ]
                ]



-- CONFIG PANE


configPaneEl : Model -> Element Msg
configPaneEl m =
    el
        [ width (fill |> minimum 480 |> maximum 860)
        , height (fill |> maximum m.env.clientHeight)
        , padding 15
        , scrollbarY
        , Font.color oneDark.text
        ]
        (configInnerEl m)


configInnerEl : Model -> Element Msg
configInnerEl m =
    column
        [ width fill
        , height fill
        , spacing 10
        ]
        [ map ProducerCtrl <| Producer.configsEl m.producerRegistry
        , if m.env.isLocalDevelopment then
            el [ width fill, alignBottom, height shrink ] <|
                map LoggerCtrl <|
                    Logger.historyEl m.log

          else
            none
        ]



-- UNSAFE STYLE


fancyScroll : Html.Html Msg
fancyScroll =
    Html.node "style" [] [ Html.text "::-webkit-scrollbar{display:none;}" ]
