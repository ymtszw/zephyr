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
        El.row [ El.width El.fill, El.height El.fill, El.clipY ]
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
            [ El.width (El.px 40)
            , El.height (El.px 40)
            , El.clip
            , Font.color oneDark.note
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just (DelColumn index), label = El.el [ El.centerX, El.centerY ] <| El.text "×" }
    )


columnAddButtonEl : Element Msg
columnAddButtonEl =
    El.el [ El.width El.fill ] <|
        Element.Input.button
            [ El.width (El.px 40)
            , El.height (El.px 40)
            , El.clip
            , Font.color oneDark.note
            , BD.dashed
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded 10
            ]
            { onPress = Just AddColumn, label = El.el [ El.centerX, El.centerY ] <| El.text "+" }


otherButtonsEl : ViewState -> Element Msg
otherButtonsEl viewState =
    El.column [ El.width El.fill, El.padding 5, El.spacingXY 0 10 ]
        [ Element.Input.button
            [ El.width (El.px 40)
            , El.height (El.px 40)
            , BD.rounded 10
            , if viewState.configOpen then
                BG.color oneDark.main

              else
                El.mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not viewState.configOpen))
            , label = El.el [ El.centerX, El.centerY ] <| octiconEl Octicons.gear
            }
        , El.link
            [ El.width (El.px 40)
            , El.height (El.px 40)
            , BD.rounded 10
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = El.el [ El.centerX, El.centerY ] <| octiconEl Octicons.markGithub
            }
        ]



-- COLUMNS


columnsEl : Model -> Element Msg
columnsEl m =
    Element.Keyed.row
        [ El.width El.fill
        , El.height (El.fill |> El.maximum m.env.clientHeight)
        , Font.regular
        ]
        (ColumnStore.indexedMap (columnKeyEl m) m.columnStore)


columnKeyEl : Model -> Int -> Column -> ( String, Element Msg )
columnKeyEl m index column =
    Tuple.pair ("column_" ++ column.id) <|
        case m.viewState.columnSwapMaybe of
            Nothing ->
                notDraggedColumnEl m index column <|
                    if m.viewState.columnSwappable then
                        [ El.htmlAttribute (draggable "true")
                        , El.htmlAttribute (style "cursor" "all-scroll")
                        , El.htmlAttribute (Html.Events.on "dragstart" (onDragStart index column.id))
                        ]

                    else
                        []

            Just swap ->
                if swap.grabbedId == column.id then
                    draggedColumnEl m.env.clientHeight

                else
                    notDraggedColumnEl m index column <|
                        [ El.htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter index, True ))) ]


notDraggedColumnEl : Model -> Int -> Column -> List (El.Attribute Msg) -> Element Msg
notDraggedColumnEl m index column attrs =
    El.column
        (columnBaseAttrs m.env.clientHeight ++ attrs)
        [ columnHeaderEl column
        , columnConfigEl m index column
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
    [ El.width (El.px 350)
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


columnConfigEl : Model -> Int -> Column -> Element Msg
columnConfigEl m index column =
    if column.configOpen then
        El.column
            [ El.width El.fill
            , El.padding 5
            , El.spacing 3
            , BG.color oneDark.sub
            ]
            [ columnConfigTitleEl "Filter Rules"
            , filtersEl m column
            , columnConfigTitleEl "Danger"
            , columnDeleteEl index column
            , Element.Input.button [ El.width El.fill, BG.color oneDark.bg ]
                { onPress = Just (ToggleColumnConfig column.id False)
                , label = octiconFreeSizeEl 24 Octicons.triangleUp
                }
            ]

    else
        El.none


columnConfigTitleEl : String -> Element Msg
columnConfigTitleEl title =
    El.el
        [ El.width El.fill
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size (scale12 3)
        , Font.color oneDark.note
        ]
        (El.text title)


filtersEl : Model -> Column -> Element Msg
filtersEl m column =
    Array.indexedMap (filterEl m column.id) column.filters
        |> Array.push (addNewFilterEl m column.id)
        |> Array.toList
        |> List.intersperse (filterLogicSeparator "AND")
        |> El.column
            [ El.width El.fill
            , El.padding 5
            , El.spacing 3
            , BD.rounded 5
            , BG.color oneDark.main
            ]


filterEl : Model -> String -> Int -> Filter -> Element Msg
filterEl m cId index filter =
    filterWrap <|
        filterGeneratorEl (SetColumnFilter cId index)
            m
            (cId ++ "filter" ++ String.fromInt index)
            (Just filter)


addNewFilterEl : Model -> String -> Element Msg
addNewFilterEl m cId =
    filterWrap <|
        filterGeneratorEl (AddColumnFilter cId)
            m
            (cId ++ "addNewFilter")
            Nothing


filterLogicSeparator : String -> Element msg
filterLogicSeparator text =
    El.el
        [ El.width El.fill
        , El.padding 3
        , Font.size (scale12 2)
        , Font.color oneDark.note
        ]
        (El.el [ El.centerX ] (El.text text))


filterWrap : Element msg -> Element msg
filterWrap =
    El.el
        [ El.width El.fill
        , El.padding 5
        , BD.width 1
        , BD.rounded 5
        , BD.color oneDark.note
        ]


filterGeneratorEl : (Filter -> Msg) -> Model -> String -> Maybe Filter -> Element Msg
filterGeneratorEl tagger m filterId filterMaybe =
    case filterMaybe of
        Just filter ->
            El.column [ El.width El.fill ] <|
                List.intersperse (filterLogicSeparator "OR") <|
                    Column.indexedMapFilter (filterAtomEl filter tagger m filterId) filter
                        ++ [ newFilterAtomEl (\fa -> tagger <| Column.appendToFilter fa filter) m filterId ]

        Nothing ->
            El.column [ El.width El.fill ]
                [ newFilterAtomEl (tagger << Singular) m filterId ]


filterAtomEl : Filter -> (Filter -> Msg) -> Model -> String -> Int -> FilterAtom -> Element Msg
filterAtomEl originalFilter tagger m filterId index filterAtom =
    let
        updateAndTag newFilterAtom =
            tagger (Column.setAtFilter index newFilterAtom originalFilter)
    in
    filterAtomInputEl updateAndTag m (filterId ++ "atom" ++ String.fromInt index) (Just filterAtom)


newFilterAtomEl : (FilterAtom -> Msg) -> Model -> String -> Element Msg
newFilterAtomEl tagger m filterId =
    filterAtomInputEl tagger m (filterId ++ "newAtom") Nothing


filterAtomInputEl : (FilterAtom -> Msg) -> Model -> String -> Maybe FilterAtom -> Element Msg
filterAtomInputEl tagger m filterAtomId filterAtomMaybe =
    let
        discordMaterial =
            Producer.discordFilterAtomMaterial m.producerRegistry
    in
    El.row [ El.width El.fill, El.spacing 3 ]
        [ filterAtomTypeSelectEl tagger m.viewState.selectState discordMaterial (filterAtomId ++ "typeSelect") filterAtomMaybe
        , filterAtomVariableInputEl tagger m.viewState.selectState discordMaterial (filterAtomId ++ "variableInput") filterAtomMaybe
        ]


filterAtomTypeSelectEl : (FilterAtom -> Msg) -> Select.State -> Discord.FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomTypeSelectEl tagger selectState discordMaterial selectId filterAtomMaybe =
    El.el [ El.width (El.fill |> El.maximum 150) ] <|
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
    El.el [ Font.size (scale12 1) ] <|
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
        , El.height (El.px 30) -- Match with select input height
        , El.padding 5
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
    Select.el
        { id = selectId
        , onSelect = tagger
        , selectedOption = Just selected
        , noMsgOptionEl = El.el [ Font.size (scale12 1) ] << optionEl
        }
        selectState
        options


mediaTypeOptionEl : MediaFilter -> Element msg
mediaTypeOptionEl mediaType =
    case mediaType of
        HasNone ->
            El.text "None"

        HasImage ->
            El.text "Image"

        HasMovie ->
            El.text "Movie"


discordGuildOptionEl : Dict String Guild -> String -> Element msg
discordGuildOptionEl guilds gId =
    case Dict.get gId guilds of
        Just guild ->
            El.row [ El.width El.fill, El.spacing 3 ]
                [ discordGuildSmallIconEl guild, El.text guild.name ]

        Nothing ->
            El.text gId


discordGuildSmallIconEl : Guild -> Element msg
discordGuildSmallIconEl guild =
    squareIconEl 20 guild.name (Maybe.map (Discord.imageUrlNoFallback (Just "16")) guild.icon)


discordChannelOptionEl : Dict String Channel -> String -> Element msg
discordChannelOptionEl channels cId =
    case Dict.get cId channels of
        Just channel ->
            case channel.guildMaybe of
                Just guild ->
                    El.row [ El.width El.fill, El.spacing 3 ]
                        [ discordGuildSmallIconEl guild, El.text ("#" ++ channel.name) ]

                Nothing ->
                    -- Mostly DM
                    El.text channel.name

        Nothing ->
            El.text cId


columnDeleteEl : Int -> Column -> Element Msg
columnDeleteEl index column =
    El.row [ El.width El.fill, El.spacing 5, El.padding 10 ]
        [ columnDeleteGateEl column
        , columnDeleteButtonEl index column
        ]


columnDeleteGateEl : Column -> Element Msg
columnDeleteGateEl column =
    Element.Input.text
        [ El.width El.fill
        , El.height (El.px 30) -- Match with select input height
        , El.padding 5
        , BG.color oneDark.note
        , BD.width 0
        , Font.size (scale12 1)
        ]
        { onChange = ColumnDeleteGateInput column.id
        , text = column.deleteGate
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    El.el [ El.centerY ] (El.text "Type DELETE to delete this column")
        , label = Element.Input.labelHidden "Delete Gate"
        }


columnDeleteButtonEl : Int -> Column -> Element Msg
columnDeleteButtonEl index column =
    El.el [ El.width (El.px 100) ] <|
        if String.toLower column.deleteGate == "delete" then
            Element.Input.button
                [ El.width El.fill
                , El.height (El.px 30)
                , BD.rounded 5
                , BG.color oneDark.err
                ]
                { onPress = Just (DelColumn index)
                , label = El.el [ El.centerX ] (El.text "Delete!")
                }

        else
            El.none



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
            squareIconEl 50 dmd.userName dmd.userAvatarUrlMaybe

        DefaultMetadata ->
            squareIconEl 50 "Zephyr" Nothing


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
