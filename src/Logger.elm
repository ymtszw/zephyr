module Logger exposing (Entry, History, Msg(..), MsgFilter(..), historyEl, init, push, update)

import Data.ColorTheme exposing (oneDark)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Extra exposing (ite)
import Html
import Html.Attributes exposing (style, tabindex)
import Octicons
import Scroll exposing (Scroll)
import View.Parts exposing (..)



-- Types


type History
    = History
        { entries : Scroll ( String, Entry )
        , payloadFilter : String
        , msgFilters : List MsgFilter
        }


type alias Entry =
    { ctor : String
    , payload : List String
    }


type MsgFilter
    = MsgFilter Bool String


init : History
init =
    History
        { entries = Scroll.defaultOptions historyElementId |> Scroll.init
        , payloadFilter = ""
        , msgFilters = defaultFilters
        }


historyElementId : String
historyElementId =
    "loggerHistory"


defaultFilters : List MsgFilter
defaultFilters =
    -- Timer ticks and text inputs are good candidates of default filters
    [ MsgFilter False "Tick"
    , MsgFilter False "Logger.ScrollStart"
    , MsgFilter False "Logger.ViewportOk"
    , MsgFilter False "Logger.FilterInput"
    , MsgFilter False "Discord.TokenInput"
    , MsgFilter False "Column.DeleteGateInput"
    ]



-- Component


type Msg
    = ScrollMsg Scroll.Msg
    | FilterInput String
    | SetMsgFilter MsgFilter
    | DelMsgFilter MsgFilter


update : Msg -> History -> ( History, Cmd Msg )
update msg (History h) =
    case msg of
        ScrollMsg sMsg ->
            let
                ( newEntries, cmd ) =
                    Scroll.update sMsg h.entries
            in
            ( History { h | entries = newEntries }, Cmd.map ScrollMsg cmd )

        FilterInput pf ->
            ( History { h | payloadFilter = pf }, Cmd.none )

        SetMsgFilter mf ->
            ( setMsgFilter mf (History h), Cmd.none )

        DelMsgFilter mf ->
            ( History { h | msgFilters = List.filter ((/=) mf) h.msgFilters }, Cmd.none )


setMsgFilter : MsgFilter -> History -> History
setMsgFilter ((MsgFilter isPos msg) as mf) (History h) =
    History <|
        if List.member mf h.msgFilters then
            h

        else if List.member (MsgFilter (not isPos) msg) h.msgFilters then
            { h
                | msgFilters =
                    h.msgFilters
                        |> List.filter ((/=) (MsgFilter (not isPos) msg))
                        |> (::) mf
            }

        else
            { h | msgFilters = mf :: h.msgFilters }



-- APIs


push : UniqueIdGen -> Entry -> History -> ( History, UniqueIdGen )
push idGen e (History h) =
    UniqueIdGen.genAndMap UniqueIdGen.logEntryPrefix idGen <|
        \eId ->
            -- Simple Pop & Dedup
            case Scroll.pop h.entries of
                ( Just ( _, d ), popped ) ->
                    if e.ctor == d.ctor then
                        History { h | entries = Scroll.push ( eId, e ) popped }

                    else
                        History { h | entries = Scroll.push ( eId, e ) h.entries }

                ( Nothing, _ ) ->
                    History { h | entries = Scroll.push ( eId, e ) h.entries }



-- View


historyEl : History -> Element Msg
historyEl h =
    column
        [ width fill
        , padding rectElementOuterPadding
        , spacing spacingUnit
        , BD.rounded rectElementRound
        , BG.color historyBackground
        , Font.size historyFontSize
        , inFront (newEntryToastEl h)
        ]
        [ historyTableEl h
        , msgFiltersEl h
        , payloadFilterInputEl h
        ]


historyBackground : Color
historyBackground =
    oneDark.sub


historyFontSize : Int
historyFontSize =
    scale12 1


historyTableEl : History -> Element Msg
historyTableEl (History h) =
    let
        ( negMsgFilters, posMsgFilters ) =
            List.partition (\(MsgFilter isPos _) -> not isPos) h.msgFilters

        payloadQueries =
            h.payloadFilter |> String.split " " |> List.filter (not << String.isEmpty)

        data =
            Scroll.toListWithFilter (filterEntry negMsgFilters posMsgFilters payloadQueries) h.entries

        columnAttrs =
            [ width fill
            , height (shrink |> maximum historyTableMaxHeight)
            , padding rectElementInnerPadding
            , spacing historyTableCellSpacing
            , clipX
            , BG.color historyTableBackground
            ]
                ++ List.map htmlAttribute (Scroll.scrollAttrs ScrollMsg h.entries)
    in
    Element.Keyed.column columnAttrs <|
        ( "LogHeaders"
        , row [ width fill, spacing historyTableCellSpacing ]
            [ el [ width ctorColumnWidth, BG.color historyTableHeaderBackground ] <| text "Msg"
            , el [ width payloadColumnWidth, BG.color historyTableHeaderBackground ] <| text "Payload"
            ]
        )
            :: List.map (rowKeyEl h.msgFilters) data


historyTableBackground : Color
historyTableBackground =
    oneDark.main


historyTableMaxHeight : Int
historyTableMaxHeight =
    400


historyTableCellSpacing : Int
historyTableCellSpacing =
    2


historyTableHeaderBackground : Color
historyTableHeaderBackground =
    oneDark.note


ctorColumnWidth : Length
ctorColumnWidth =
    fillPortion 1


payloadColumnWidth : Length
payloadColumnWidth =
    fillPortion 2


filterEntry : List MsgFilter -> List MsgFilter -> List String -> ( String, Entry ) -> Bool
filterEntry negMsgFilters posMsgFilters payloadQueries ( _, e ) =
    if negMsgFilters == [] || not (List.member (MsgFilter False e.ctor) negMsgFilters) then
        if posMsgFilters == [] || List.member (MsgFilter True e.ctor) posMsgFilters then
            payloadQueries == [] || List.any (\q -> List.any (String.contains q) e.payload) payloadQueries

        else
            False

    else
        False


newEntryToastEl : History -> Element Msg
newEntryToastEl (History h) =
    let
        size =
            Scroll.pendingSize h.entries
    in
    el [ width fill, alignTop, padding rectElementOuterPadding, visible (size /= 0) ] <|
        Element.Input.button [ width fill, padding rectElementInnerPadding, BG.color oneDark.succ ]
            { onPress = Just (ScrollMsg Scroll.BackToTop)
            , label = el [ centerX ] <| text ("New Log Entry (" ++ String.fromInt size ++ ")")
            }


rowKeyEl : List MsgFilter -> ( String, Entry ) -> ( String, Element Msg )
rowKeyEl msgFilters ( eId, e ) =
    ( eId
    , row [ width fill, spacing historyTableCellSpacing ]
        [ el [ alignTop, width ctorColumnWidth ] <| ctorCellEl msgFilters e
        , el [ alignTop, width payloadColumnWidth ] <| payloadCellsEl e
        ]
    )


ctorCellEl : List MsgFilter -> Entry -> Element Msg
ctorCellEl msgFilters entry =
    row [ spacing spacingUnit ]
        [ breakP [ bold ] [ breakT entry.ctor ]
        , Element.Input.button [ htmlAttribute (tabindex -1) ] <|
            if List.member (MsgFilter True entry.ctor) msgFilters then
                { onPress = Just (DelMsgFilter (MsgFilter True entry.ctor))
                , label = el [ BG.color oneDark.succ ] <| ctorFilterButtonEl Octicons.diffAdded
                }

            else
                { onPress = Just (SetMsgFilter (MsgFilter True entry.ctor))
                , label = el [] <| ctorFilterButtonEl Octicons.diffAdded
                }
        , Element.Input.button [ htmlAttribute (tabindex -1) ] <|
            -- No need for switch since if Negative Filter is set, this entry should be invisible
            { onPress = Just (SetMsgFilter (MsgFilter False entry.ctor))
            , label = el [] <| ctorFilterButtonEl Octicons.diffRemoved
            }
        ]


ctorFilterButtonEl : (Octicons.Options -> Html.Html Msg) -> Element Msg
ctorFilterButtonEl shape =
    octiconEl
        { size = ctorFilterButtonSize
        , color = defaultOcticonColor
        , shape = shape
        }


ctorFilterButtonSize : Int
ctorFilterButtonSize =
    scale12 1


payloadCellsEl : Entry -> Element Msg
payloadCellsEl entry =
    column [ width fill, spacing historyTableCellSpacing ] (List.map payloadCellEl entry.payload)


payloadCellEl : String -> Element Msg
payloadCellEl raw =
    breakP
        [ width fill
        , height (shrink |> maximum payloadCellMaxHeight)
        , padding rectElementInnerPadding
        , scrollbarY
        , BG.color payloadCellBackground
        , Font.family [ Font.typeface "Lucida Console", Font.typeface "Monaco", Font.monospace ]
        , htmlAttribute (style "user-select" "all")
        ]
        [ breakT raw ]


payloadCellBackground : Color
payloadCellBackground =
    oneDark.bg


payloadCellMaxHeight : Int
payloadCellMaxHeight =
    100


msgFiltersEl : History -> Element Msg
msgFiltersEl (History h) =
    wrappedRow [ width fill, spacing spacingUnit ] <|
        List.map msgFilterEl h.msgFilters


msgFilterEl : MsgFilter -> Element Msg
msgFilterEl ((MsgFilter isPos ctor) as mf) =
    row
        [ width shrink
        , BD.rounded rectElementRound
        , BG.color (ite isPos oneDark.succ oneDark.err)
        ]
        [ el
            [ padding msgFilterPadding
            , BD.roundEach { topLeft = rectElementRound, bottomLeft = rectElementRound, topRight = 0, bottomRight = 0 }
            ]
            (text ctor)
        , Element.Input.button
            [ padding msgFilterPadding
            , focused []
            , htmlAttribute (tabindex -1)
            , BD.roundEach { topLeft = 0, bottomLeft = 0, topRight = rectElementRound, bottomRight = rectElementRound }
            ]
            { onPress = Just (DelMsgFilter mf)
            , label = octiconEl { size = msgFilterDeleteIconSize, color = msgFilterDeleteIconColor, shape = Octicons.trashcan }
            }
        ]


msgFilterPadding : Int
msgFilterPadding =
    2


msgFilterDeleteIconSize : Int
msgFilterDeleteIconSize =
    scale12 2


msgFilterDeleteIconColor : Color
msgFilterDeleteIconColor =
    oneDark.text


payloadFilterInputEl : History -> Element Msg
payloadFilterInputEl (History h) =
    textInputEl
        { onChange = FilterInput
        , theme = oneDark
        , enabled = True
        , text = h.payloadFilter
        , label = Element.Input.labelHidden "Log Payload Filter"
        , placeholder = Just (text "Payload OR Filter (Space-delimited, Case-sensitive)")
        }
