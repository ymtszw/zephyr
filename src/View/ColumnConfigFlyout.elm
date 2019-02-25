module View.ColumnConfigFlyout exposing (columnConfigFlyoutEl)

import Array
import Data.ColorTheme exposing (ColorTheme, aubergine, oneDark)
import Data.Column as Column exposing (Msg(..))
import Data.Filter as Filter exposing (Filter(..), FilterAtom(..), MediaFilter(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Lazy exposing (..)
import List.Extra
import Octicons
import Scroll
import String exposing (fromInt)
import StringExtra
import View.Parts exposing (..)
import View.Select as Select exposing (select)


columnConfigFlyoutEl : ColorTheme -> Select.State -> FilterAtomMaterial -> Int -> Column.Column -> Element Msg
columnConfigFlyoutEl theme ss fam index c =
    column
        [ width fill
        , alignTop
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , visible c.configOpen
        , BG.color (flyoutBackground theme)
        , BD.width 1
        , BD.color (flyoutFrameColor theme)
        , Font.size baseFontSize
        ]
        [ lazy statusHeaderEl theme
        , lazy4 statusEl theme (Scroll.size c.items) c.pinned c.id
        , lazy3 filterSectionHeaderEl theme c.id (c.filters /= c.pendingFilters)
        , lazy4 filtersEl theme ss fam c
        , lazy dangerZoneHeaderEl theme
        , columnDeleteEl theme c
        , lazy2 columnConfigCloseButtonEl theme c.id
        ]


baseFontSize : Int
baseFontSize =
    scale12 1


flyoutBackground : ColorTheme -> Color
flyoutBackground =
    .sub


flyoutFrameColor : ColorTheme -> Color
flyoutFrameColor =
    .note


statusHeaderEl : ColorTheme -> Element Msg
statusHeaderEl theme =
    row
        [ width fill
        , padding titlePadding
        , spacing spacingUnit
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size titleFontSize
        , Font.color (flyoutFrameColor theme)
        ]
        [ octiconEl [] { size = titleFontSize, color = theme.succ, shape = Octicons.pulse }
        , text "Status"
        ]


titleFontSize : Int
titleFontSize =
    scale12 3


titlePadding : Int
titlePadding =
    2


statusEl : ColorTheme -> Int -> Bool -> String -> Element Msg
statusEl theme nItems pinned cId =
    [ [ "ID", cId ]
    , [ "Stored messages", StringExtra.punctuateNumber nItems ]
    , [ "Pinned"
      , if pinned then
            "Yes"

        else
            "No"
      ]
    ]
        |> List.map (List.intersperse " - " >> List.map text >> row [ spacing spacingUnit ])
        |> column
            [ width (fill |> minimum 0)
            , padding rectElementInnerPadding
            , spacing spacingUnit
            , BD.rounded rectElementRound
            , BG.color (sectionBackground theme)
            ]


filterSectionHeaderEl : ColorTheme -> String -> Bool -> Element Msg
filterSectionHeaderEl theme cId isDirty =
    row
        [ width fill
        , padding titlePadding
        , spacing spacingUnit
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size titleFontSize
        , Font.color (flyoutFrameColor theme)
        ]
        [ octiconEl [] { size = titleFontSize, color = theme.prim, shape = Octicons.rocket }
        , text "Filter Rules"
        , thinButtonEl [ alignRight, Font.size baseFontSize ]
            { onPress = ColumnCtrl cId ConfirmFilter
            , width = shrink |> minimum 60
            , enabledColor = theme.succ
            , enabledFontColor = theme.text
            , enabled = isDirty
            , innerElement = text "Apply"
            }
        ]


dangerZoneHeaderEl : ColorTheme -> Element Msg
dangerZoneHeaderEl theme =
    row
        [ width fill
        , padding titlePadding
        , spacing spacingUnit
        , BD.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Font.size titleFontSize
        , Font.color (flyoutFrameColor theme)
        ]
        [ octiconEl [] { size = titleFontSize, color = theme.err, shape = Octicons.stop }
        , text "Danger Zone"
        ]


filtersEl : ColorTheme -> Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
filtersEl theme ss fam c =
    Array.indexedMap (\fi f -> filterEditorEl theme ss fam c.id (EditF fi f)) c.pendingFilters
        |> Array.push (filterEditorEl theme ss fam c.id AddF)
        |> Array.toList
        |> List.intersperse (filterLogicSeparator theme "AND")
        |> List.append
            [ if Array.isEmpty c.filters then
                none

              else
                filterHelpEl theme
            ]
        |> column
            [ width (fill |> minimum 0)
            , padding rectElementInnerPadding
            , spacing spacingUnit
            , BD.rounded rectElementRound
            , BG.color (sectionBackground theme)
            ]


sectionBackground : ColorTheme -> Color
sectionBackground =
    .main


filterHelpEl : ColorTheme -> Element Msg
filterHelpEl theme =
    breakTColumn [ width fill, Font.color (flyoutFrameColor theme) ]
        [ breakP []
            [ el [ Font.color theme.warn, Font.italic ] (breakT "CAUTION: ")
            , breakT "Messages currently stored in this column will be "
            , el [ Font.color theme.err ] (breakT "discarded")
            , breakT " when filter rules are updated."
            ]
        , breakP []
            [ breakT "After the update is "
            , el [ BG.color theme.succ, Font.color theme.text, paddingXY spacingUnit 0 ] (breakT "Applied,")
            , breakT " messages still available in the local buffer are re-read and inserted to this column according to the new filter rules."
            ]
        ]


type FEditorType
    = EditF Int Filter
    | AddF


filterEditorEl : ColorTheme -> Select.State -> FilterAtomMaterial -> String -> FEditorType -> Element Msg
filterEditorEl theme ss fam cId editorType =
    row
        [ width fill
        , BD.width 1
        , BD.rounded rectElementRound
        , BD.color (flyoutFrameColor theme)
        ]
        [ case editorType of
            EditF fi filter ->
                Filter.indexedMap (\ai fa -> filterAtomEditorEl theme ss fam cId (EditFA fi ai fa)) filter
                    ++ [ filterAtomEditorEl theme ss fam cId (AddFA fi) ]
                    |> List.intersperse (filterLogicSeparator theme "OR")
                    |> column
                        [ width (fill |> minimum 0)
                        , padding rectElementInnerPadding
                        , spacing spacingUnit
                        ]

            AddF ->
                column [ width (fill |> minimum 0), padding rectElementInnerPadding ]
                    [ filterAtomEditorEl theme ss fam cId GenFilter
                    ]
        , deleteFilterButtonEl theme cId editorType
        ]


deleteFilterButtonEl : ColorTheme -> String -> FEditorType -> Element Msg
deleteFilterButtonEl theme cId editorType =
    case editorType of
        EditF fi _ ->
            Element.Input.button
                [ width (px deleteFilterButtonWidth)
                , height fill
                , mouseOver [ BG.color theme.err ]
                , focused [ BG.color theme.err ]
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
                    octiconEl [ centerY, centerX ]
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


filterLogicSeparator : ColorTheme -> String -> Element msg
filterLogicSeparator theme operator =
    el
        [ width (fill |> minimum 0)
        , Font.size separatorFontSize
        , Font.color (flyoutFrameColor theme)
        ]
        (el [ centerX ] (text operator))


separatorFontSize : Int
separatorFontSize =
    scale12 2


type FAInputType
    = EditFA Int Int FilterAtom
    | AddFA Int
    | GenFilter


filterAtomEditorEl : ColorTheme -> Select.State -> FilterAtomMaterial -> String -> FAInputType -> Element Msg
filterAtomEditorEl theme ss fam cId faInputType =
    row [ width (fill |> minimum 0), spacing spacingUnit ] <|
        case faInputType of
            EditFA fi ai fa ->
                [ filterAtomCtorSelectEl theme ss fam cId faInputType
                , filterAtomVariableInputEl theme ss fam cId fi ai fa
                ]

            _ ->
                [ filterAtomCtorSelectEl theme ss fam cId faInputType ]


filterAtomCtorSelectEl : ColorTheme -> Select.State -> FilterAtomMaterial -> String -> FAInputType -> Element Msg
filterAtomCtorSelectEl theme selectState fam cId faInputType =
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
    select [ width (px filterAtomCtorFixedWidth) ]
        { state = selectState
        , msgTagger = SelectCtrl
        , id = selectId
        , theme = theme
        , thin = False
        , onSelect = filterAtomOnSelect cId faInputType
        , selectedOption = selectedOption
        , filterMatch = Nothing
        , options = availableFilterAtomsWithDefaultArguments fam faInputType
        , optionEl = filterAtomCtorOptionEl
        }


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
            text "Discord message in ..."

        OfSlackConversation _ ->
            text "Slack message in ..."

        ByMessage _ ->
            text "Message contains ..."

        ByMedia _ ->
            text "Attached media ..."

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
                                ( OfDiscordChannel _, OfDiscordChannel _ ) ->
                                    selected

                                ( OfSlackConversation _, OfSlackConversation _ ) ->
                                    selected

                                ( ByMessage _, ByMessage _ ) ->
                                    selected

                                ( ByMedia _, ByMedia _ ) ->
                                    selected

                                _ ->
                                    option
                    in
                    List.map replaceIfSameType options ++ [ RemoveMe ]

                _ ->
                    options

        serviceFilterAtoms =
            List.filterMap identity
                [ Maybe.map Tuple.first fam.ofDiscordChannel
                , Maybe.map .default fam.ofSlackConversation
                ]
    in
    (serviceFilterAtoms ++ [ ByMessage "text", ByMedia HasImage ])
        |> editByInputType
        |> List.map (\fa -> ( ctorKey fa, fa ))


ctorKey : FilterAtom -> String
ctorKey fa =
    case fa of
        OfDiscordChannel _ ->
            "OfDiscordChannel"

        OfSlackConversation _ ->
            "OfSlackConversation"

        ByMessage _ ->
            "ByMessage"

        ByMedia _ ->
            "ByMedia"

        RemoveMe ->
            "RemoveMe"


filterAtomVariableInputEl : ColorTheme -> Select.State -> FilterAtomMaterial -> String -> Int -> Int -> FilterAtom -> Element Msg
filterAtomVariableInputEl theme ss fam columnId fi ai fa =
    let
        favsOptions =
            FAVSOptions columnId fi ai ss
    in
    case fa of
        OfDiscordChannel channelId ->
            let
                ( selectedMaybe, options ) =
                    Maybe.withDefault ( Nothing, [] ) (Maybe.map prepareOptions fam.ofDiscordChannel)

                prepareOptions ( _, channels ) =
                    ( List.Extra.find (.id >> (==) channelId) channels
                    , List.map (\c -> ( c.id, c )) channels
                    )
            in
            filterAtomVariableSelectEl theme (OfDiscordChannel << .id) <|
                favsOptions selectedMaybe (Just Discord.channelFilter) options <|
                    \c -> discordChannelEl [] { size = favsIconSize, channel = c }

        OfSlackConversation convIdStr ->
            let
                ( selectedMaybe, options ) =
                    Maybe.withDefault ( Nothing, [] ) (Maybe.map prepareOptions fam.ofSlackConversation)

                prepareOptions { conversations } =
                    ( List.Extra.find (Slack.getConversationIdStr >> (==) convIdStr) conversations
                    , List.map (\c -> ( Slack.getConversationIdStr c, c )) conversations
                    )
            in
            filterAtomVariableSelectEl theme (OfSlackConversation << Slack.getConversationIdStr) <|
                favsOptions selectedMaybe (Just slackConvCacheFilter) options <|
                    \c -> slackConversationEl [] { fontSize = baseFontSize, conversation = c, team = Just ( c.team, favsIconSize ) }

        ByMessage query ->
            filterAtomVariableTextInputEl theme ByMessage columnId fi ai query

        ByMedia mediaType ->
            let
                options =
                    [ ( "HasImage", HasImage ), ( "HasVideo", HasVideo ), ( "HasNone", HasNone ) ]
            in
            filterAtomVariableSelectEl theme ByMedia <|
                favsOptions (Just mediaType) Nothing options mediaTypeOptionEl

        RemoveMe ->
            -- Should not happen
            none


favsIconSize : Int
favsIconSize =
    20


slackConvCacheFilter : String -> Slack.ConversationCache -> Bool
slackConvCacheFilter f c =
    StringExtra.containsCaseIgnored f c.name || StringExtra.containsCaseIgnored f c.team.name


filterAtomVariableTextInputEl : ColorTheme -> (String -> FilterAtom) -> String -> Int -> Int -> String -> Element Msg
filterAtomVariableTextInputEl theme faTagger cId fi ai current =
    textInputEl []
        { onChange = \str -> ColumnCtrl cId (SetFilterAtom { filterIndex = fi, atomIndex = ai, atom = faTagger str })
        , theme = theme
        , enabled = True
        , text = current
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Filter Text"
        }


type alias FAVSOptions a =
    { columnId : String
    , filterIndex : Int
    , atomIndex : Int
    , state : Select.State
    , selected : Maybe a
    , filterMatch : Maybe (String -> a -> Bool)
    , options : List ( String, a )
    , optionEl : a -> Element Msg
    }


filterAtomVariableSelectEl : ColorTheme -> (a -> FilterAtom) -> FAVSOptions a -> Element Msg
filterAtomVariableSelectEl theme faTagger opts =
    select [ width fill ]
        { state = opts.state
        , msgTagger = SelectCtrl
        , id = filterAtomVariableSelectId opts
        , theme = theme
        , thin = False
        , onSelect = onSelectFilterAtomVariable faTagger opts
        , selectedOption = opts.selected
        , filterMatch = opts.filterMatch
        , options = opts.options
        , optionEl = opts.optionEl
        }


filterAtomVariableSelectId : FAVSOptions a -> String
filterAtomVariableSelectId opts =
    opts.columnId ++ "-filter_" ++ fromInt opts.filterIndex ++ "-atom_" ++ fromInt opts.atomIndex ++ "_variable"


onSelectFilterAtomVariable : (a -> FilterAtom) -> FAVSOptions a -> a -> Msg
onSelectFilterAtomVariable faTagger opts var =
    ColumnCtrl opts.columnId <|
        SetFilterAtom { filterIndex = opts.filterIndex, atomIndex = opts.atomIndex, atom = faTagger var }


mediaTypeOptionEl : MediaFilter -> Element msg
mediaTypeOptionEl mediaType =
    case mediaType of
        HasImage ->
            text "Image"

        HasVideo ->
            text "Video"

        HasNone ->
            text "None"


columnDeleteEl : ColorTheme -> Column.Column -> Element Msg
columnDeleteEl theme c =
    row
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BG.color (sectionBackground theme)
        , BD.rounded rectElementRound
        ]
        [ columnDeleteGateEl theme c.id c.deleteGate
        , lazy3 columnDeleteButtonEl theme c.id (String.toLower c.deleteGate == "delete")
        ]


columnDeleteGateEl : ColorTheme -> String -> String -> Element Msg
columnDeleteGateEl theme cId deleteGate =
    textInputEl []
        { onChange = ColumnCtrl cId << DeleteGateInput
        , theme = theme
        , enabled = True
        , text = deleteGate
        , label = Element.Input.labelHidden "Delete Confirmation"
        , placeholder = Just (text "Type DELETE to delete this column")
        }


columnDeleteButtonEl : ColorTheme -> String -> Bool -> Element Msg
columnDeleteButtonEl theme cId confirmed =
    dangerButtonEl []
        { onPress = DelColumn cId
        , width = px deleteButtonWidth
        , theme = theme
        , enabled = confirmed
        , innerElement =
            text <|
                if confirmed then
                    "Delete!"

                else
                    "Delete?"
        }


deleteButtonWidth : Int
deleteButtonWidth =
    100


columnConfigCloseButtonEl : ColorTheme -> String -> Element Msg
columnConfigCloseButtonEl theme cId =
    Element.Input.button [ width fill, BG.color theme.sub ]
        { onPress = Just (ColumnCtrl cId (Column.ToggleConfig False))
        , label = octiconEl [ centerX ] { size = closeTriangleSize, color = defaultOcticonColor, shape = Octicons.triangleUp }
        }


closeTriangleSize : Int
closeTriangleSize =
    24
