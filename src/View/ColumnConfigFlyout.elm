module View.ColumnConfigFlyout exposing (columnConfigFlyoutEl)

import Array
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column exposing (Msg(..))
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
import Html.Attributes exposing (style)
import ListExtra
import Octicons
import String exposing (fromInt)
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
            [ lazy2 filterSectionHeaderEl c.id (c.filters /= c.pendingFilters)
            , lazy3 filtersEl ss fam c
            , dangerZoneHeaderEl
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


filterSectionHeaderEl : String -> Bool -> Element Msg
filterSectionHeaderEl cId isDirty =
    row
        [ width fill
        , padding titlePadding
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size titleFontSize
        , Font.color flyoutFrameColor
        ]
        [ text "Filter Rules"
        , el [ alignRight, Font.size baseFontSize ] <|
            thinButtonEl
                { onPress = ColumnCtrl cId ConfirmFilter
                , width = shrink |> minimum 60
                , enabledColor = oneDark.succ
                , enabledFontColor = oneDark.text
                , disabledColor = flyoutBackground
                , disabledFontColor = flyoutFrameColor
                , enabled = isDirty
                , innerElement = text "Apply"
                }
        ]


titleFontSize : Int
titleFontSize =
    scale12 3


titlePadding : Int
titlePadding =
    2


dangerZoneHeaderEl : Element Msg
dangerZoneHeaderEl =
    el
        [ width fill
        , padding titlePadding
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size titleFontSize
        , Font.color flyoutFrameColor
        ]
        (text "Danger Zone")


filtersEl : Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
filtersEl ss fam c =
    Array.indexedMap (\fi f -> filterEditorEl ss fam c.id (EditF fi f)) c.pendingFilters
        |> Array.push (filterEditorEl ss fam c.id AddF)
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


type FEditorType
    = EditF Int Filter
    | AddF


filterEditorEl : Select.State -> FilterAtomMaterial -> String -> FEditorType -> Element Msg
filterEditorEl ss fam cId editorType =
    row
        [ width fill
        , BD.width 1
        , BD.rounded rectElementRound
        , BD.color flyoutFrameColor
        ]
        [ case editorType of
            EditF fi filter ->
                Filter.indexedMap (\ai fa -> filterAtomEditorEl ss fam cId (EditFA fi ai fa)) filter
                    ++ [ filterAtomEditorEl ss fam cId (AddFA fi) ]
                    |> List.intersperse (filterLogicSeparator "OR")
                    |> column
                        [ width (fill |> minimum 0)
                        , padding rectElementInnerPadding
                        , spacing spacingUnit
                        ]

            AddF ->
                column [ width (fill |> minimum 0), padding rectElementInnerPadding ]
                    [ filterAtomEditorEl ss fam cId GenFilter
                    ]
        , deleteFilterButtonEl cId editorType
        ]


deleteFilterButtonEl : String -> FEditorType -> Element Msg
deleteFilterButtonEl cId editorType =
    case editorType of
        EditF fi _ ->
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
                { onPress = Just (ColumnCtrl cId (DelFilter fi))
                , label =
                    el [ centerY, centerX ] <|
                        octiconEl
                            { size = deleteFilterIconSize
                            , color = defaultOcticonColor
                            , shape = Octicons.trashcan
                            }
                }

        AddF ->
            none


deleteFilterButtonWidth : Int
deleteFilterButtonWidth =
    20


deleteFilterIconSize : Int
deleteFilterIconSize =
    18


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


type FAInputType
    = EditFA Int Int FilterAtom
    | AddFA Int
    | GenFilter


filterAtomEditorEl : Select.State -> FilterAtomMaterial -> String -> FAInputType -> Element Msg
filterAtomEditorEl ss fam cId faInputType =
    row [ width (fill |> minimum 0), spacing spacingUnit ] <|
        case faInputType of
            EditFA fi ai fa ->
                [ filterAtomCtorSelectEl ss fam cId faInputType
                , filterAtomVariableInputEl ss fam cId fi ai fa
                ]

            _ ->
                [ filterAtomCtorSelectEl ss fam cId faInputType ]


filterAtomCtorSelectEl : Select.State -> FilterAtomMaterial -> String -> FAInputType -> Element Msg
filterAtomCtorSelectEl selectState fam cId faInputType =
    let
        ( selectId, selectedOption ) =
            case faInputType of
                EditFA fi ai fa ->
                    ( cId ++ "-filter_" ++ fromInt fi ++ "-atom_" ++ fromInt ai ++ "_ctor", Just fa )

                AddFA fi ->
                    ( cId ++ "-filter_" ++ fromInt fi ++ "-newAtom", Nothing )

                GenFilter ->
                    ( cId ++ "-newFilter", Nothing )
    in
    el [ width (px filterAtomCtorFixedWidth) ] <|
        select
            { id = selectId
            , theme = oneDark
            , onSelect = filterAtomOnSelect cId faInputType
            , selectedOption = selectedOption
            , noMsgOptionEl = filterAtomCtorOptionEl
            }
            selectState
            (availableFilterAtomsWithDefaultArguments fam faInputType)


filterAtomCtorFixedWidth : Int
filterAtomCtorFixedWidth =
    120


filterAtomOnSelect : String -> FAInputType -> FilterAtom -> Msg
filterAtomOnSelect cId faInputType newFa =
    case ( faInputType, newFa ) of
        ( EditFA fi ai _, RemoveMe ) ->
            ColumnCtrl cId (DelFilterAtom { filterIndex = fi, atomIndex = ai })

        ( EditFA fi ai _, _ ) ->
            ColumnCtrl cId (SetFilterAtom { filterIndex = fi, atomIndex = ai, atom = newFa })

        ( AddFA fi, RemoveMe ) ->
            -- Should not happen
            NoOp

        ( AddFA fi, _ ) ->
            ColumnCtrl cId (AddFilterAtom { filterIndex = fi, atom = newFa })

        ( GenFilter, RemoveMe ) ->
            -- Should not happen
            NoOp

        ( GenFilter, _ ) ->
            ColumnCtrl cId (AddFilter (Singular newFa))


filterAtomCtorOptionEl : FilterAtom -> Element msg
filterAtomCtorOptionEl filterAtom =
    case filterAtom of
        OfDiscordChannel _ ->
            text "Discord message in channel..."

        ByMessage _ ->
            text "Message contains..."

        ByMedia _ ->
            text "Attached media..."

        RemoveMe ->
            text "Remove this filter"


availableFilterAtomsWithDefaultArguments : FilterAtomMaterial -> FAInputType -> List ( String, FilterAtom )
availableFilterAtomsWithDefaultArguments fam faInputType =
    let
        editByInputType options =
            case faInputType of
                EditFA _ _ selected ->
                    let
                        replaceIfSameType option =
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
                    List.map replaceIfSameType options ++ [ RemoveMe ]

                _ ->
                    options
    in
    List.filterMap identity [ Maybe.map Tuple.first fam.ofDiscordChannel ]
        |> List.append [ ByMessage "text", ByMedia HasImage ]
        |> editByInputType
        |> List.map (\fa -> ( ctorKey fa, fa ))


ctorKey : FilterAtom -> String
ctorKey fa =
    case fa of
        OfDiscordChannel _ ->
            "OfDiscordChannel"

        ByMessage _ ->
            "ByMessage"

        ByMedia _ ->
            "ByMedia"

        RemoveMe ->
            "RemoveMe"


filterAtomVariableInputEl : Select.State -> FilterAtomMaterial -> String -> Int -> Int -> FilterAtom -> Element Msg
filterAtomVariableInputEl ss fam cId fi ai fa =
    case fa of
        OfDiscordChannel channelId ->
            let
                channelSelectEl selected options =
                    filterAtomVariableSelectEl (OfDiscordChannel << .id) ss cId fi ai selected options

                fallbackChannel =
                    Discord.unavailableChannel channelId
            in
            case fam.ofDiscordChannel of
                Just ( _, channels ) ->
                    let
                        selectedChannel =
                            channels |> ListExtra.findOne (\c -> c.id == channelId) |> Maybe.withDefault fallbackChannel
                    in
                    channelSelectEl selectedChannel <|
                        ( List.map (\c -> ( c.id, c )) channels, discordChannelEl discordGuildIconSize )

                Nothing ->
                    channelSelectEl fallbackChannel ( [], always none )

        ByMessage query ->
            filterAtomVariableTextInputEl ByMessage cId fi ai query

        ByMedia mediaType ->
            filterAtomVariableSelectEl ByMedia ss cId fi ai mediaType <|
                ( [ ( "HasImage", HasImage ), ( "HasMovie", HasMovie ), ( "HasNone", HasNone ) ], mediaTypeOptionEl )

        RemoveMe ->
            -- Should not happen
            none


discordGuildIconSize : Int
discordGuildIconSize =
    20


filterAtomVariableTextInputEl : (String -> FilterAtom) -> String -> Int -> Int -> String -> Element Msg
filterAtomVariableTextInputEl tagger cId fi ai current =
    textInputEl
        { onChange = \str -> ColumnCtrl cId (SetFilterAtom { filterIndex = fi, atomIndex = ai, atom = tagger str })
        , theme = oneDark
        , enabled = True
        , text = current
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Filter Text"
        }


filterAtomVariableSelectEl :
    (a -> FilterAtom)
    -> Select.State
    -> String
    -> Int
    -> Int
    -> a
    -> ( List ( String, a ), a -> Element Msg )
    -> Element Msg
filterAtomVariableSelectEl tagger selectState cId fi ai selected ( options, optionEl ) =
    select
        { id = cId ++ "-filter_" ++ fromInt fi ++ "-atom_" ++ fromInt ai ++ "_variable"
        , theme = oneDark
        , onSelect = \option -> ColumnCtrl cId (SetFilterAtom { filterIndex = fi, atomIndex = ai, atom = tagger option })
        , selectedOption = Just selected
        , noMsgOptionEl = optionEl
        }
        selectState
        options


mediaTypeOptionEl : MediaFilter -> Element msg
mediaTypeOptionEl mediaType =
    case mediaType of
        HasImage ->
            text "Image"

        HasMovie ->
            text "Movie"

        HasNone ->
            text "None"


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
        { onChange = ColumnCtrl cId << DeleteGateInput
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
        { onPress = Just (ColumnCtrl cId (Column.ToggleConfig False))
        , label = octiconEl { size = closeTriangleSize, color = defaultOcticonColor, shape = Octicons.triangleUp }
        }


closeTriangleSize : Int
closeTriangleSize =
    24
