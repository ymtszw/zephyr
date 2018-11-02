module View.ColumnConfigFlyout exposing (columnConfigFlyoutEl)

import Array
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.Filter as Filter exposing (Filter(..), FilterAtom(..), MediaFilter(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Lazy exposing (..)
import Extra exposing (ite)
import ListExtra
import Octicons
import View.Parts exposing (..)
import View.Select as Select exposing (select)


columnConfigFlyoutEl : Select.State -> FilterAtomMaterial -> Int -> Column.Column -> Element Msg
columnConfigFlyoutEl ss fam index c =
    if c.configOpen then
        column
            [ width fill
            , alignTop
            , padding rectElementInnerPadding
            , spacing spacingUnit
            , BG.color flyoutBackground
            , BD.width 1
            , BD.color flyoutFrameColor
            , Font.size baseFontSize
            ]
            [ lazy columnConfigTitleEl "Filter Rules"
            , lazy3 filtersEl ss fam c
            , lazy columnConfigTitleEl "Danger Zone"
            , columnDeleteEl index c
            , lazy columnConfigCloseButtonEl c.id
            ]

    else
        none


baseFontSize : Int
baseFontSize =
    scale12 1


flyoutBackground : Color
flyoutBackground =
    oneDark.sub


flyoutFrameColor : Color
flyoutFrameColor =
    oneDark.note


columnConfigTitleEl : String -> Element Msg
columnConfigTitleEl title =
    el
        [ width fill
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size titleFontSize
        , Font.color flyoutFrameColor
        ]
        (text title)


titleFontSize : Int
titleFontSize =
    scale12 3


filtersEl : Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
filtersEl ss fam c =
    Array.indexedMap (filterEl ss fam c.id) c.filters
        |> Array.push (addNewFilterEl ss fam c.id)
        |> Array.toList
        |> List.intersperse (filterLogicSeparator "AND")
        |> column
            [ width (fill |> minimum 0)
            , padding rectElementInnerPadding
            , spacing spacingUnit
            , BD.rounded rectElementRound
            , BG.color sectionBackground
            ]


sectionBackground : Color
sectionBackground =
    oneDark.main


filterEl : Select.State -> FilterAtomMaterial -> String -> Int -> Filter -> Element Msg
filterEl ss fam cId index filter =
    let
        tagger newFilterMaybe =
            case newFilterMaybe of
                Just newFilter ->
                    SetColumnFilter cId index newFilter

                Nothing ->
                    DelColumnFilter cId index
    in
    filterGeneratorEl tagger ss fam cId (Just ( index, filter ))


addNewFilterEl : Select.State -> FilterAtomMaterial -> String -> Element Msg
addNewFilterEl ss fam cId =
    let
        tagger newFilterMaybe =
            case newFilterMaybe of
                Just newFilter ->
                    AddColumnFilter cId newFilter

                Nothing ->
                    -- Should not happen
                    tagger newFilterMaybe
    in
    filterGeneratorEl tagger ss fam cId Nothing


filterLogicSeparator : String -> Element msg
filterLogicSeparator operator =
    el
        [ width (fill |> minimum 0)
        , Font.size separatorFontSize
        , Font.color flyoutFrameColor
        ]
        (el [ centerX ] (text operator))


separatorFontSize : Int
separatorFontSize =
    scale12 2


filterGeneratorEl : (Maybe Filter -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Maybe ( Int, Filter ) -> Element Msg
filterGeneratorEl tagger ss fam cId indexFilterMaybe =
    row
        [ width fill
        , BD.width 1
        , BD.rounded rectElementRound
        , BD.color flyoutFrameColor
        ]
        [ case indexFilterMaybe of
            Just ( index, filter ) ->
                let
                    filterId =
                        cId ++ "-filter_" ++ String.fromInt index
                in
                Filter.indexedMap (filterAtomEl filter tagger ss fam filterId) filter
                    ++ [ newFilterAtomEl (\fa -> tagger (Just (Filter.append fa filter))) ss fam filterId ]
                    |> List.intersperse (filterLogicSeparator "OR")
                    |> column
                        [ width (fill |> minimum 0)
                        , padding rectElementInnerPadding
                        , spacing spacingUnit
                        ]

            Nothing ->
                column [ width (fill |> minimum 0), padding rectElementInnerPadding ]
                    [ newFilterAtomEl (tagger << Just << Singular) ss fam (cId ++ "addNewFilter") ]
        , deleteFilterButtonEl cId indexFilterMaybe
        ]


deleteFilterButtonEl : String -> Maybe ( Int, Filter ) -> Element Msg
deleteFilterButtonEl cId indexFilterMaybe =
    case indexFilterMaybe of
        Just ( index, _ ) ->
            Element.Input.button
                [ width (px deleteFilterButtonWidth)
                , height fill
                , mouseOver [ BG.color oneDark.err ]
                , focused [ BG.color oneDark.err ]
                , alignRight
                , BD.roundEach
                    { topLeft = 0
                    , topRight = rectElementRound
                    , bottomRight = rectElementRound
                    , bottomLeft = 0
                    }
                ]
                { onPress = Just (DelColumnFilter cId index)
                , label = el [ centerY, centerX ] <| octiconFreeSizeEl deleteFilterIconSize Octicons.trashcan
                }

        Nothing ->
            none


deleteFilterButtonWidth : Int
deleteFilterButtonWidth =
    20


deleteFilterIconSize : Int
deleteFilterIconSize =
    18


filterAtomEl : Filter -> (Maybe Filter -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Int -> FilterAtom -> Element Msg
filterAtomEl originalFilter tagger ss fam filterId index filterAtom =
    let
        updateAndTag newFilterAtom =
            tagger <|
                case newFilterAtom of
                    RemoveMe ->
                        Filter.removeAt index originalFilter

                    _ ->
                        Just (Filter.setAt index newFilterAtom originalFilter)
    in
    filterAtomInputEl updateAndTag ss fam (filterId ++ "-atom_" ++ String.fromInt index) (Just filterAtom)


newFilterAtomEl : (FilterAtom -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Element Msg
newFilterAtomEl tagger ss fam filterId =
    filterAtomInputEl tagger ss fam (filterId ++ "newAtom") Nothing


filterAtomInputEl : (FilterAtom -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomInputEl tagger ss fam filterAtomId filterAtomMaybe =
    row [ width (fill |> minimum 0), spacing spacingUnit ]
        [ filterAtomCtorSelectEl tagger ss fam (filterAtomId ++ "-typeSelect") filterAtomMaybe
        , filterAtomVariableInputEl tagger ss fam (filterAtomId ++ "-variableInput") filterAtomMaybe
        ]


filterAtomCtorSelectEl : (FilterAtom -> Msg) -> Select.State -> FilterAtomMaterial -> String -> Maybe FilterAtom -> Element Msg
filterAtomCtorSelectEl tagger selectState material selectId filterAtomMaybe =
    el [ width (px filterAtomCtorFixedWidth) ] <|
        select
            { id = selectId
            , theme = oneDark
            , onSelect = tagger
            , selectedOption = filterAtomMaybe
            , noMsgOptionEl = filterAtomCtorOptionEl
            }
            selectState
            (availableFilterAtomsWithDefaultArguments material filterAtomMaybe)


filterAtomCtorFixedWidth : Int
filterAtomCtorFixedWidth =
    120


filterAtomCtorOptionEl : FilterAtom -> Element msg
filterAtomCtorOptionEl filterAtom =
    case filterAtom of
        ByMessage _ ->
            text "Message contains..."

        ByMedia _ ->
            text "Attached media..."

        OfDiscordChannel _ ->
            text "Discord message in channel..."

        RemoveMe ->
            text "Remove this filter"


availableFilterAtomsWithDefaultArguments : FilterAtomMaterial -> Maybe FilterAtom -> List ( String, FilterAtom )
availableFilterAtomsWithDefaultArguments material filterAtomMaybe =
    replaceWithSelected filterAtomMaybe
        (basicFilterAtoms ++ materialToDefaultFilterAtoms material)
        ++ Maybe.withDefault [] (Maybe.map (always [ RemoveMe ]) filterAtomMaybe)
        |> List.map (\fa -> ( ctorKey fa, fa ))


basicFilterAtoms : List FilterAtom
basicFilterAtoms =
    [ ByMessage "text"
    , ByMedia HasNone
    ]


ctorKey : FilterAtom -> String
ctorKey fa =
    case fa of
        ByMessage _ ->
            "ByMessage"

        ByMedia _ ->
            "ByMedia"

        OfDiscordChannel _ ->
            "OfDiscordChannel"

        RemoveMe ->
            "RemoveMe"


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
                ( [ ( "HasNone", HasNone ), ( "HasImage", HasImage ), ( "HasMovie", HasMovie ) ], mediaTypeOptionEl )

        Just (OfDiscordChannel cId) ->
            filterAtomVariableSelectInputEl (tagger << OfDiscordChannel) selectState (inputId ++ "-variableSelect") cId <|
                case material.ofDiscordChannel of
                    Just ( _, channels ) ->
                        ( List.map (\c -> ( c.id, c.id )) channels, discordChannelWithGuildIconEl channels )

                    Nothing ->
                        ( [], text )

        Just RemoveMe ->
            -- Should not happen
            none

        Nothing ->
            none


filterAtomVariableTextInputEl : (String -> Msg) -> String -> Element Msg
filterAtomVariableTextInputEl tagger text =
    textInputEl
        { onChange = tagger
        , theme = oneDark
        , enabled = True
        , text = text
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Filter Text"
        }


filterAtomVariableSelectInputEl : (a -> Msg) -> Select.State -> String -> a -> ( List ( String, a ), a -> Element Msg ) -> Element Msg
filterAtomVariableSelectInputEl tagger selectState selectId selected ( options, optionEl ) =
    select
        { id = selectId
        , theme = oneDark
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
            text "None"

        HasImage ->
            text "Image"

        HasMovie ->
            text "Movie"


discordChannelWithGuildIconEl : List Discord.ChannelCache -> String -> Element msg
discordChannelWithGuildIconEl channels cId =
    case ListExtra.findOne (.id >> (==) cId) channels of
        Just channel ->
            row [ width (fill |> minimum 0), spacingXY discordGuildIconSpacingX 0 ]
                [ channel.guildMaybe |> Maybe.map (discordGuildIconEl discordGuildIconSize) |> Maybe.withDefault none
                , breakP [] [ breakT ("#" ++ channel.name) ]
                ]

        Nothing ->
            text cId


discordGuildIconSpacingX : Int
discordGuildIconSpacingX =
    2


discordGuildIconSize : Int
discordGuildIconSize =
    20


columnDeleteEl : Int -> Column.Column -> Element Msg
columnDeleteEl index c =
    row
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BG.color sectionBackground
        , BD.rounded rectElementRound
        ]
        [ columnDeleteGateEl c.id c.deleteGate
        , lazy2 columnDeleteButtonEl index (String.toLower c.deleteGate == "delete")
        ]


columnDeleteGateEl : String -> String -> Element Msg
columnDeleteGateEl cId deleteGate =
    textInputEl
        { onChange = ColumnDeleteGateInput cId
        , theme = oneDark
        , enabled = True
        , text = deleteGate
        , label = Element.Input.labelHidden "Delete Confirmation"
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    el [ centerY ] (text "Type DELETE to delete this column")
        }


columnDeleteButtonEl : Int -> Bool -> Element Msg
columnDeleteButtonEl index confirmed =
    dangerButtonEl
        { onPress = DelColumn index
        , width = px deleteButtonWidth
        , theme = oneDark
        , enabled = confirmed
        , innerElement = text (ite confirmed "Delete!" "Delete?")
        }


deleteButtonWidth : Int
deleteButtonWidth =
    100


columnConfigCloseButtonEl : String -> Element Msg
columnConfigCloseButtonEl cId =
    Element.Input.button [ width fill, BG.color oneDark.sub ]
        { onPress = Just (ToggleColumnConfig cId False)
        , label = octiconFreeSizeEl closeTriangleSize Octicons.triangleUp
        }


closeTriangleSize : Int
closeTriangleSize =
    24
