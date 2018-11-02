module View.Sidebar exposing (sidebarEl)

import Array exposing (Array)
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter exposing (Filter, FilterAtom(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import ListExtra
import Octicons
import Url
import View.Parts exposing (..)


sidebarEl : Model -> Element Msg
sidebarEl { columnStore, viewState, env } =
    column
        [ width (px (buttonSize + paddingX * 2))
        , height (fill |> maximum env.clientHeight)
        , alignLeft
        , paddingXY paddingX spacingY
        , BG.color oneDark.bg
        ]
        [ el [ width fill, alignTop ] (columnButtonsEl viewState.filterAtomMaterial columnStore)
        , el [ width fill, alignBottom ] (lazy otherButtonsEl viewState.configOpen)
        ]


buttonSize : Int
buttonSize =
    40


paddingX : Int
paddingX =
    5


spacingY : Int
spacingY =
    10


columnButtonsEl : FilterAtomMaterial -> ColumnStore -> Element Msg
columnButtonsEl fam columnStore =
    Element.Keyed.column [ width fill, paddingXY 0 spacingY, spacingXY 0 spacingY, Font.color oneDark.text ] <|
        (columnAddButtonKeyEl :: ColumnStore.indexedMap (columnButtonKeyEl fam) columnStore)


columnButtonKeyEl : FilterAtomMaterial -> Int -> Column.Column -> ( String, Element Msg )
columnButtonKeyEl fam index { id, filters } =
    filters
        |> Array.foldl (filterToIconUrl fam) Nothing
        |> Maybe.withDefault defaultColumnIconEl
        |> asColumnButton index id
        |> Tuple.pair ("sidebarButton_" ++ id)


asColumnButton : Int -> String -> Element Msg -> Element Msg
asColumnButton index cId element =
    Element.Input.button [ width (px buttonSize), height (px buttonSize) ]
        { onPress = Just (RevealColumn index)
        , label = element
        }


filterToIconUrl : FilterAtomMaterial -> Filter -> Maybe (Element Msg) -> Maybe (Element Msg)
filterToIconUrl fam filter urlMaybe =
    let
        reducer filterAtom acc =
            case ( acc, filterAtom ) of
                ( Just _, _ ) ->
                    acc

                ( _, OfDiscordChannel cId ) ->
                    fam.ofDiscordChannel
                        |> Maybe.andThen (\( _, channels ) -> ListExtra.findOne (.id >> (==) cId) channels)
                        |> Maybe.map discordChannelIconEl

                ( _, _ ) ->
                    Nothing
    in
    Data.Filter.fold reducer urlMaybe filter


discordChannelIconEl : Discord.ChannelCache -> Element Msg
discordChannelIconEl c =
    case c.guildMaybe of
        Just guild ->
            iconWithBadgeEl
                { size = buttonSize
                , badge = Just discordBadgeEl
                , fallback = c.name
                , url = Maybe.map (Discord.imageUrlNoFallback (Just buttonSize)) guild.icon
                }

        Nothing ->
            iconWithBadgeEl
                { size = buttonSize
                , badge = Nothing
                , fallback = c.name
                , url = Just (Discord.defaultIconUrl (Just buttonSize))
                }


discordBadgeEl : Element Msg
discordBadgeEl =
    let
        badgeSize =
            buttonSize // 3
    in
    el
        [ width (px badgeSize)
        , height (px badgeSize)
        , padding 1
        , BD.rounded 2
        , BG.uncropped (Discord.defaultIconUrl (Just badgeSize))
        ]
        none


defaultColumnIconEl : Element Msg
defaultColumnIconEl =
    el
        [ width (px buttonSize)
        , height (px buttonSize)
        , clip
        , BD.width 1
        , BD.color oneDark.note
        , BD.rounded 10
        , Font.size (buttonSize - 10)
        , Font.family [ Font.serif ]
        ]
        (el [ centerX, centerY ] <| text "Z")


columnAddButtonKeyEl : ( String, Element Msg )
columnAddButtonKeyEl =
    Element.Input.button
        [ width (px buttonSize)
        , height (px buttonSize)
        , clip
        , Font.color oneDark.note
        , BD.dashed
        , BD.width 1
        , BD.color oneDark.note
        , BD.rounded 10
        ]
        { onPress = Just AddColumn
        , label =
            el [ centerX, centerY ] <|
                octiconFreeSizeEl (buttonSize // 2) Octicons.plus
        }
        |> el [ width fill ]
        |> Tuple.pair "columnAddButton"


otherButtonsEl : Bool -> Element Msg
otherButtonsEl configOpen =
    column [ width fill, paddingXY 0 spacingY, spacingXY 0 spacingY ]
        [ Element.Input.button
            [ width (px buttonSize)
            , height (px buttonSize)
            , BD.rounded 10
            , if configOpen then
                BG.color oneDark.main

              else
                mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not configOpen))
            , label = el [ centerX, centerY ] <| octiconEl Octicons.gear
            }
        , newTabLink
            [ width (px buttonSize)
            , height (px buttonSize)
            , BD.rounded 10
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = el [ centerX, centerY ] <| octiconEl Octicons.markGithub
            }
        ]
