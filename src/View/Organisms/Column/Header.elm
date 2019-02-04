module View.Organisms.Column.Header exposing (Effects, Source(..), render)

import Html exposing (Html, div)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode exposing (succeed)
import View.Atoms.Background as Background
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon


type alias Effects msg =
    { onDragstart : Bool -> Int -> String -> msg
    , onPinButtonClick : String -> Bool -> msg
    , onConfigToggleButtonClick : String -> Bool -> msg
    , onDismissButtonClick : Int -> msg
    }


type Source
    = DiscordSource { channelName : String, guildIcon : Maybe String }
    | SlackSource { convName : String, teamIcon : Maybe String }


render :
    Effects msg
    -> Int
    ->
        { c
            | id : String
            , sources : List Source
            , filters : List String
            , pinned : Bool
            , configOpen : Bool
        }
    -> Html msg
render eff index column =
    div
        [ flexRow
        , flexBasisAuto
        , spacingRow2
        , Background.colorSub
        ]
        [ grabbableIcon (eff.onDragstart column.pinned index column.id) column
        ]


grabbableIcon : msg -> { c | id : String, sources : List Source } -> Html msg
grabbableIcon onDragstart column =
    div
        [ flexBasisAuto
        , padding5
        , draggable "true"
        , on "dragstart" (succeed onDragstart)
        ]
        [ sourceIcon column.sources
        ]


sourceIcon : List Source -> Html msg
sourceIcon sources =
    case sources of
        [] ->
            Icon.abbr [ Icon.rounded40, serif, sizeTitle ] "Zephyr"

        s :: _ ->
            let
                ( bottomRight, content ) =
                    case s of
                        DiscordSource opts ->
                            ( Icon.discordBadge14
                            , Icon.imgOrAbbr [ Icon.rounded40, serif, sizeTitle ] opts.channelName opts.guildIcon
                            )

                        SlackSource opts ->
                            ( Icon.slackBadge14
                            , Icon.imgOrAbbr [ Icon.rounded40, serif, sizeTitle ] opts.convName opts.teamIcon
                            )
            in
            withBadge [ badgeOutset ]
                { topRight = Nothing
                , bottomRight = Just bottomRight
                , content = content
                }
