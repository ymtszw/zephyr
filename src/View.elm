module View exposing (body)

import Array exposing (Array)
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column exposing (Column, Filter(..), FilterAtom(..), MediaFilter(..), MetadataFilter(..))
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item, Media(..), Metadata(..))
import Data.Model exposing (ColumnSwap, Model, ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord exposing (Channel, Guild)
import Data.TextRenderer exposing (TextRenderer)
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Region exposing (description)
import Html
import Html.Attributes exposing (draggable, style)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Octicons
import String exposing (fromFloat)
import Url
import View.Parts exposing (octiconEl, octiconFreeSizeEl, scale12, squareIconEl)
import View.Select as Select


body : Model -> List (Html.Html Msg)
body m =
    [ El.layout (dragEventHandlers m.viewState.columnSwapMaybe) (bodyEl m)
    , fancyScroll
    ]


bodyEl : Model -> Element Msg
bodyEl model =
    backgroundEl <|
        El.row [ El.width El.fill, El.height El.fill ]
            [ sidebarEl model
            , if model.viewState.configOpen then
                configPaneEl model

              else
                El.none
            , columnsEl model
            ]


backgroundEl : Element Msg -> Element Msg
backgroundEl contents =
    El.row
        [ BG.color oneDark.bg
        , El.width El.fill
        , El.height El.fill
        , El.inFront contents
        ]
        [ El.el
            [ El.centerY
            , El.centerX
            , Font.bold
            , Font.color oneDark.sub
            , Font.size (scale12 12)
            , Font.center
            , Font.family [ Font.serif ]
            ]
            (El.text "Zephyr")
        ]


dragEventHandlers : Maybe ColumnSwap -> List (El.Attribute Msg)
dragEventHandlers columnSwapMaybe =
    case columnSwapMaybe of
        Just _ ->
            [ El.htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))
            , El.htmlAttribute (Html.Events.preventDefaultOn "dragover" (D.succeed ( NoOp, True )))
            , El.htmlAttribute (Html.Events.preventDefaultOn "drop" (D.succeed ( NoOp, True )))
            ]

        Nothing ->
            []



-- SIDEBAR


sidebarEl : Model -> Element Msg
sidebarEl { columnStore, viewState, env } =
    El.column
        [ El.width (El.px 50)
        , El.height (El.fill |> El.maximum env.clientHeight)
        , El.paddingXY 0 10
        , BG.color oneDark.bg
        ]
        [ El.el [ El.width El.fill, El.alignTop ] (columnButtonsEl columnStore)
        , El.el [ El.width El.fill, El.alignBottom ] (otherButtonsEl viewState)
        ]


columnButtonsEl : ColumnStore -> Element Msg
columnButtonsEl columnStore =
    List.append (ColumnStore.indexedMap columnButtonEl columnStore) [ ( "columnAddButton", columnAddButtonEl ) ]
        |> Element.Keyed.column [ El.width El.fill, El.padding 5, El.spacingXY 0 10 ]


columnButtonEl : Int -> Column -> ( String, Element Msg )
columnButtonEl index { id } =
    ( "sidebarButton_" ++ id
    , El.el [ El.width El.fill ] <|
        Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 10
            , El.clip
            , Font.color oneDark.note
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just (DelColumn index), label = El.text "×" }
    )


columnAddButtonEl : Element Msg
columnAddButtonEl =
    El.el [ El.width El.fill ] <|
        Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 10
            , El.clip
            , Font.color oneDark.note
            , BD.dashed
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just AddColumn, label = El.text "+" }


otherButtonsEl : ViewState -> Element Msg
otherButtonsEl viewState =
    El.column [ El.width El.fill, El.padding 5, El.spacingXY 0 10 ]
        [ Element.Input.button
            [ El.width El.fill
            , El.paddingXY 0 7
            , BD.rounded 10
            , if viewState.configOpen then
                BG.color oneDark.main

              else
                El.mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not viewState.configOpen))
            , label = octiconEl Octicons.gear
            }
        , El.link
            [ El.width El.fill
            , El.paddingXY 0 7
            , BD.rounded 10
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = octiconEl Octicons.markGithub
            }
        ]



-- COLUMNS


columnsEl : Model -> Element Msg
columnsEl { columnStore, producerRegistry, viewState, env } =
    Element.Keyed.row
        [ El.width El.fill
        , El.height (El.fill |> El.maximum env.clientHeight)
        , Font.regular
        ]
        (ColumnStore.indexedMap (columnKeyEl env.clientHeight viewState producerRegistry) columnStore)


columnKeyEl : Int -> ViewState -> ProducerRegistry -> Int -> Column -> ( String, Element Msg )
columnKeyEl clientHeight { columnSwappable, columnSwapMaybe, selectState } producerRegistry index column =
    Tuple.pair ("column_" ++ column.id) <|
        case columnSwapMaybe of
            Nothing ->
                notDraggedColumnEl clientHeight selectState producerRegistry column <|
                    if columnSwappable then
                        [ El.htmlAttribute (draggable "true")
                        , El.htmlAttribute (style "cursor" "all-scroll")
                        , El.htmlAttribute (Html.Events.on "dragstart" (onDragStart index column.id))
                        ]

                    else
                        []

            Just swap ->
                if swap.grabbedId == column.id then
                    draggedColumnEl clientHeight

                else
                    notDraggedColumnEl clientHeight selectState producerRegistry column <|
                        [ El.htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter index, True ))) ]


notDraggedColumnEl : Int -> Select.State -> ProducerRegistry -> Column -> List (El.Attribute Msg) -> Element Msg
notDraggedColumnEl clientHeight selectState producerRegistry column attrs =
    El.column
        (columnBaseAttrs clientHeight ++ attrs)
        [ columnHeaderEl column
        , columnConfigEl selectState producerRegistry column
        , column.items
            |> List.map itemEl
            |> El.column
                [ El.width El.fill
                , El.paddingXY 5 0
                , El.scrollbarY
                ]
        ]


draggedColumnEl : Int -> Element Msg
draggedColumnEl clientHeight =
    El.el (columnBaseAttrs clientHeight ++ [ BG.color oneDark.bg ]) El.none


columnBaseAttrs : Int -> List (El.Attribute Msg)
columnBaseAttrs clientHeight =
    [ El.width (El.fill |> El.minimum 320 |> El.maximum 860)
    , El.height (El.fill |> El.maximum clientHeight)
    , BG.color oneDark.main
    , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
    , BD.color oneDark.bg
    , Font.color oneDark.text
    ]


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


columnHeaderEl : Column -> Element Msg
columnHeaderEl column =
    El.row
        [ El.width El.fill
        , El.padding 10
        , BG.color oneDark.sub
        ]
        [ El.text ("[PH] " ++ column.id)
        , Element.Input.button [ El.alignRight ]
            { onPress = Just (ToggleColumnConfig column.id (not column.configOpen))
            , label = octiconEl Octicons.settings
            }
        ]


columnConfigEl : Select.State -> ProducerRegistry -> Column -> Element Msg
columnConfigEl selectState producerRegistry column =
    if column.configOpen then
        El.el
            [ El.width El.fill
            , BG.color oneDark.sub
            , El.padding 5
            ]
            (filtersEl selectState producerRegistry column)

    else
        El.none


filtersEl : Select.State -> ProducerRegistry -> Column -> Element Msg
filtersEl selectState producerRegistry column =
    Array.indexedMap (filterEl selectState producerRegistry column.id) column.filters
        |> Array.push (addNewFilterEl selectState producerRegistry column.id)
        |> Array.toList
        |> El.column
            [ El.width El.fill
            , El.padding 5
            , El.spacing 3
            , BD.rounded 5
            , BG.color oneDark.main
            ]


filterEl : Select.State -> ProducerRegistry -> String -> Int -> Filter -> Element Msg
filterEl selectState producerRegistry cId index filter =
    filterWrap <|
        filterGeneratorEl (SetColumnFilter cId index)
            selectState
            producerRegistry
            (cId ++ "filter" ++ String.fromInt index)
            (Just filter)


addNewFilterEl : Select.State -> ProducerRegistry -> String -> Element Msg
addNewFilterEl selectState producerRegistry cId =
    filterWrap <|
        filterGeneratorEl (AddColumnFilter cId)
            selectState
            producerRegistry
            (cId ++ "addNewFilter")
            Nothing


filterWrap : Element msg -> Element msg
filterWrap =
    El.el
        [ El.width El.fill
        , El.padding 5
        , BD.width 1
        , BD.rounded 5
        , BD.color oneDark.note
        ]


filterGeneratorEl : (Filter -> Msg) -> Select.State -> ProducerRegistry -> String -> Maybe Filter -> Element Msg
filterGeneratorEl tagger selectState producerRegistry filterId filterMaybe =
    case filterMaybe of
        Just filter ->
            El.column [ El.width El.fill ] <|
                Column.indexedMapFilter (filterAtomEl filter tagger selectState producerRegistry filterId) filter
                    ++ [ newFilterAtomEl (\fa -> tagger <| Column.appendToFilter fa filter) selectState producerRegistry filterId ]

        Nothing ->
            El.column [ El.width El.fill ]
                [ newFilterAtomEl (tagger << Singular) selectState producerRegistry filterId ]


filterAtomEl : Filter -> (Filter -> Msg) -> Select.State -> ProducerRegistry -> String -> Int -> FilterAtom -> Element Msg
filterAtomEl originalFilter tagger selectState producerRegistry filterId index filterAtom =
    let
        updateAndTag newFilterAtom =
            tagger (Column.setAtFilter index newFilterAtom originalFilter)
    in
    filterAtomInputEl updateAndTag selectState producerRegistry (filterId ++ "atom" ++ String.fromInt index) (Just filterAtom)


newFilterAtomEl : (FilterAtom -> Msg) -> Select.State -> ProducerRegistry -> String -> Element Msg
newFilterAtomEl tagger selectState producerRegistry filterId =
    filterAtomInputEl tagger selectState producerRegistry (filterId ++ "newAtom") Nothing


filterAtomInputEl : (FilterAtom -> Msg) -> Select.State -> ProducerRegistry -> String -> Maybe FilterAtom -> Element Msg
filterAtomInputEl tagger selectState producerRegistry filterAtomId filterAtomMaybe =
    let
        discordMaterial =
            Producer.discordFilterAtomMaterial producerRegistry
    in
    El.row [ El.width El.fill, El.spacing 5 ]
        [ filterAtomTypeSelectEl tagger selectState discordMaterial (filterAtomId ++ "typeSelect") filterAtomMaybe
        , filterAtomVariableInputEl tagger selectState discordMaterial (filterAtomId ++ "variableInput") filterAtomMaybe
        ]


filterAtomTypeSelectEl : (FilterAtom -> Msg) -> Select.State -> Discord.FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomTypeSelectEl tagger selectState discordMaterial selectId filterAtomMaybe =
    Select.el
        { id = selectId
        , onSelect = tagger
        , selectedOption = filterAtomMaybe
        , noMsgOptionEl = filterAtomTypeOptionEl
        }
        selectState
        (availableFilterAtomsWithDefaultArguments discordMaterial filterAtomMaybe)


filterAtomTypeOptionEl : FilterAtom -> Element msg
filterAtomTypeOptionEl filterAtom =
    case filterAtom of
        ByMessage _ ->
            El.text "Message contains..."

        ByMedia _ ->
            El.text "Attached media..."

        ByMetadata IsDefault ->
            El.text "System message"

        ByMetadata IsDiscord ->
            El.text "Discord message"

        ByMetadata (OfDiscordGuild _) ->
            El.text "Discord message in server..."

        ByMetadata (OfDiscordChannel _) ->
            El.text "Discord message in channel..."


availableFilterAtomsWithDefaultArguments : Discord.FilterAtomMaterial -> Maybe FilterAtom -> List FilterAtom
availableFilterAtomsWithDefaultArguments discordMaterial filterAtomMaybe =
    basicFilterAtoms filterAtomMaybe ++ discordFilterAtoms discordMaterial filterAtomMaybe


basicFilterAtoms : Maybe FilterAtom -> List FilterAtom
basicFilterAtoms filterAtomMaybe =
    replaceWithSelected filterAtomMaybe
        [ ByMessage "text"
        , ByMedia HasNone
        , ByMetadata IsDefault
        ]


discordFilterAtoms : Discord.FilterAtomMaterial -> Maybe FilterAtom -> List FilterAtom
discordFilterAtoms { isDiscord, ofDiscordGuild, ofDiscordChannel } filterAtomMaybe =
    replaceWithSelected filterAtomMaybe <|
        List.filterMap identity <|
            [ isDiscord
            , Maybe.map Tuple.first ofDiscordGuild
            , Maybe.map Tuple.first ofDiscordChannel
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

                        ( ByMetadata IsDefault, ByMetadata IsDefault ) ->
                            selected

                        ( ByMetadata IsDiscord, ByMetadata IsDiscord ) ->
                            selected

                        ( ByMetadata (OfDiscordGuild _), ByMetadata (OfDiscordGuild _) ) ->
                            selected

                        ( ByMetadata (OfDiscordChannel _), ByMetadata (OfDiscordChannel _) ) ->
                            selected

                        _ ->
                            option
            in
            List.map (replaceIfSameType filterAtom) filterAtoms

        Nothing ->
            filterAtoms


filterAtomVariableInputEl : (FilterAtom -> Msg) -> Select.State -> Discord.FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomVariableInputEl tagger selectState discordMaterial inputId filterAtomMaybe =
    case filterAtomMaybe of
        Just (ByMessage query) ->
            filterAtomVariableTextInputEl (tagger << ByMessage) query

        Just (ByMedia mediaType) ->
            filterAtomVariableSelectInputEl (tagger << ByMedia) selectState (inputId ++ "variableSelect") mediaType <|
                ( [ HasNone, HasImage, HasMovie ], mediaTypeOptionEl )

        Just (ByMetadata IsDefault) ->
            El.none

        Just (ByMetadata IsDiscord) ->
            El.none

        Just (ByMetadata (OfDiscordGuild gId)) ->
            filterAtomVariableSelectInputEl (tagger << ByMetadata << OfDiscordGuild) selectState (inputId ++ "variableSelect") gId <|
                case discordMaterial.ofDiscordGuild of
                    Just ( _, guilds ) ->
                        ( Dict.keys guilds, discordGuildOptionEl guilds )

                    Nothing ->
                        ( [], El.text )

        Just (ByMetadata (OfDiscordChannel cId)) ->
            filterAtomVariableSelectInputEl (tagger << ByMetadata << OfDiscordChannel) selectState (inputId ++ "variableSelect") cId <|
                case discordMaterial.ofDiscordChannel of
                    Just ( _, channels ) ->
                        ( Dict.values channels
                            |> List.sortBy (.guildMaybe >> Maybe.map .name >> Maybe.withDefault "~~~")
                            |> List.map .id
                          -- Tilde is sorted AFTER "z" in ordinary sort algorithms, suitable for fallback
                        , discordChannelOptionEl channels
                        )

                    Nothing ->
                        ( [], El.text )

        Nothing ->
            El.none


filterAtomVariableTextInputEl : (String -> Msg) -> String -> Element Msg
filterAtomVariableTextInputEl tagger text =
    Element.Input.text
        [ El.width El.fill
        , El.padding 5
        , BG.color oneDark.note
        , BD.width 0
        ]
        { onChange = tagger
        , text = text
        , placeholder = Nothing
        , label = Element.Input.labelLeft [] El.none
        }


filterAtomVariableSelectInputEl : (a -> Msg) -> Select.State -> String -> a -> ( List a, a -> Element Msg ) -> Element Msg
filterAtomVariableSelectInputEl tagger selectState selectId selected ( options, optionEl ) =
    Select.el
        { id = selectId
        , onSelect = tagger
        , selectedOption = Just selected
        , noMsgOptionEl = optionEl
        }
        selectState
        options


mediaTypeOptionEl : MediaFilter -> Element msg
mediaTypeOptionEl mediaType =
    case mediaType of
        HasNone ->
            El.row [ El.width El.fill ] [ El.text "None" ]

        HasImage ->
            El.row [ El.width El.fill ] [ El.text "Image", El.el [ El.alignRight ] (octiconFreeSizeEl 18 Octicons.fileMedia) ]

        HasMovie ->
            El.row [ El.width El.fill ] [ El.text "Movie", El.el [ El.alignRight ] (octiconFreeSizeEl 18 Octicons.deviceCameraVideo) ]


discordGuildOptionEl : Dict String Guild -> String -> Element msg
discordGuildOptionEl guilds gId =
    case Dict.get gId guilds of
        Just guild ->
            El.text guild.name

        Nothing ->
            El.text gId


discordChannelOptionEl : Dict String Channel -> String -> Element msg
discordChannelOptionEl channels cId =
    case Dict.get cId channels of
        Just channel ->
            case channel.guildMaybe of
                Just guild ->
                    El.text (guild.name ++ " / #" ++ channel.name)

                Nothing ->
                    -- Mostly DM
                    El.text channel.name

        Nothing ->
            El.text cId



-- ITEM


itemEl : Item -> Element Msg
itemEl item =
    El.row
        [ El.width El.fill
        , El.paddingXY 0 5
        , El.spacing 5
        , BD.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , BD.color oneDark.bd
        ]
        [ itemAvatarEl item
        , itemContentsEl item
        ]


itemAvatarEl : Item -> Element Msg
itemAvatarEl item =
    case item.metadata of
        DiscordMetadata dmd ->
            squareIconEl dmd.userName dmd.userAvatarUrlMaybe

        DefaultMetadata ->
            squareIconEl "Zephyr" Nothing


itemContentsEl : Item -> Element Msg
itemContentsEl item =
    case item.mediaMaybe of
        Just media ->
            El.textColumn [ El.spacingXY 0 10, El.width El.fill, El.alignTop ]
                [ messageToParagraph item.message
                , mediaEl media
                ]

        Nothing ->
            El.el [ El.width El.fill, El.alignTop ] (messageToParagraph item.message)


messageToParagraph : String -> Element Msg
messageToParagraph message =
    El.paragraph
        [ Font.size (scale12 2)
        , El.htmlAttribute (style "white-space" "pre-wrap")
        ]
        (Data.TextRenderer.default oneDark message)


mediaEl : Media -> Element Msg
mediaEl media =
    case media of
        Image url ->
            El.image [ El.width El.fill ]
                -- TODO pass description
                { src = Url.toString url, description = "Welcome image" }

        Movie _ ->
            -- Placeholder
            El.none



-- CONFIG PANE


configPaneEl : Model -> Element Msg
configPaneEl m =
    El.el
        [ El.width (El.fill |> El.minimum 480 |> El.maximum 860)
        , El.height (El.fill |> El.maximum m.env.clientHeight)
        , El.padding 15
        , El.scrollbarY
        , Font.color oneDark.text
        ]
        (configInnerEl m)


configInnerEl : Model -> Element Msg
configInnerEl m =
    El.column
        [ El.width El.fill
        , El.height El.fill
        ]
        [ El.map ProducerCtrl <| Producer.configsEl m.producerRegistry
        ]



-- UNSAFE STYLE


fancyScroll : Html.Html Msg
fancyScroll =
    Html.node "style" [] [ Html.text "::-webkit-scrollbar{display:none;}" ]
