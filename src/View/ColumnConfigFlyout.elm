module View.ColumnConfigFlyout exposing (columnConfigFlyoutEl)

import Array
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.Filter as Filter exposing (Filter(..), FilterAtom(..), MediaFilter(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model exposing (Model, ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Lazy exposing (lazy, lazy2)
import ListExtra
import Octicons
import View.Parts exposing (..)
import View.Select as Select exposing (select)


columnConfigFlyoutEl : Model -> Int -> Column.Column -> Element Msg
columnConfigFlyoutEl m index c =
    if c.configOpen then
        column
            [ width fill
            , padding 5
            , spacing 3
            , BG.color oneDark.sub
            , BD.width 1
            , BD.color oneDark.note
            ]
            [ lazy columnConfigTitleEl "Filter Rules"
            , lazy2 filtersEl m.viewState c
            , lazy columnConfigTitleEl "Danger Zone"
            , columnDeleteEl index c
            , lazy columnConfigCloseButtonEl c.id
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


filtersEl : ViewState -> Column.Column -> Element Msg
filtersEl vs c =
    Array.indexedMap (filterEl vs c.id) c.filters
        |> Array.push (addNewFilterEl vs c.id)
        |> Array.toList
        |> List.intersperse (filterLogicSeparator "AND")
        |> column
            [ width (fill |> minimum 0)
            , padding 5
            , spacing 3
            , BD.rounded 5
            , BG.color oneDark.main
            ]


filterEl : ViewState -> String -> Int -> Filter -> Element Msg
filterEl vs cId index filter =
    let
        tagger newFilterMaybe =
            case newFilterMaybe of
                Just newFilter ->
                    SetColumnFilter cId index newFilter

                Nothing ->
                    DelColumnFilter cId index
    in
    filterGeneratorEl tagger vs cId (Just ( index, filter ))


addNewFilterEl : ViewState -> String -> Element Msg
addNewFilterEl vs cId =
    let
        tagger newFilterMaybe =
            case newFilterMaybe of
                Just newFilter ->
                    AddColumnFilter cId newFilter

                Nothing ->
                    -- Should not happen
                    tagger newFilterMaybe
    in
    filterGeneratorEl tagger vs cId Nothing


filterLogicSeparator : String -> Element msg
filterLogicSeparator operator =
    el
        [ width (fill |> minimum 0)
        , padding 3
        , Font.size (scale12 2)
        , Font.color oneDark.note
        ]
        (el [ centerX ] (text operator))


filterGeneratorEl : (Maybe Filter -> Msg) -> ViewState -> String -> Maybe ( Int, Filter ) -> Element Msg
filterGeneratorEl tagger vs cId indexFilterMaybe =
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
                        Filter.indexedMap (filterAtomEl filter tagger vs filterId) filter
                            ++ [ newFilterAtomEl (\fa -> tagger (Just (Filter.append fa filter))) vs filterId ]

            Nothing ->
                column [ width (fill |> minimum 0), padding 5 ]
                    [ newFilterAtomEl (tagger << Just << Singular) vs (cId ++ "addNewFilter") ]
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


filterAtomEl : Filter -> (Maybe Filter -> Msg) -> ViewState -> String -> Int -> FilterAtom -> Element Msg
filterAtomEl originalFilter tagger vs filterId index filterAtom =
    let
        updateAndTag newFilterAtom =
            tagger <|
                case newFilterAtom of
                    RemoveMe ->
                        Filter.removeAt index originalFilter

                    _ ->
                        Just (Filter.setAt index newFilterAtom originalFilter)
    in
    filterAtomInputEl updateAndTag vs (filterId ++ "-atom_" ++ String.fromInt index) (Just filterAtom)


newFilterAtomEl : (FilterAtom -> Msg) -> ViewState -> String -> Element Msg
newFilterAtomEl tagger vs filterId =
    filterAtomInputEl tagger vs (filterId ++ "newAtom") Nothing


filterAtomInputEl : (FilterAtom -> Msg) -> ViewState -> String -> Maybe FilterAtom -> Element Msg
filterAtomInputEl tagger vs filterAtomId filterAtomMaybe =
    row [ width (fill |> minimum 0), spacing 3 ]
        [ filterAtomTypeSelectEl tagger vs.selectState vs.filterAtomMaterial (filterAtomId ++ "-typeSelect") filterAtomMaybe
        , filterAtomVariableInputEl tagger vs.selectState vs.filterAtomMaterial (filterAtomId ++ "-variableInput") filterAtomMaybe
        ]


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
                        ( List.map .id channels, lazy2 discordChannelWithGuildIconEl channels )

                    Nothing ->
                        ( [], text )

        Just RemoveMe ->
            -- Should not happen
            none

        Nothing ->
            none


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


discordChannelWithGuildIconEl : List Discord.ChannelCache -> String -> Element msg
discordChannelWithGuildIconEl channels cId =
    case ListExtra.findOne (.id >> (==) cId) channels of
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


columnConfigCloseButtonEl : String -> Element Msg
columnConfigCloseButtonEl cId =
    Element.Input.button [ width fill, BG.color oneDark.sub ]
        { onPress = Just (ToggleColumnConfig cId False)
        , label = octiconFreeSizeEl 24 Octicons.triangleUp
        }
