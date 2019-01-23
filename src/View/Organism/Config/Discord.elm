module View.Organism.Config.Discord exposing (CurrentState(..), Effects, Props, render, styles)

import Color exposing (cssRgba)
import Data.Producer.Discord as Discord
import Dict
import Html exposing (Html, button, div, h3, img, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Octicons
import View.Atom.Animation as Animation
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image exposing (octiconPathStyle)
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    , onRehydrateButtonClick : msg
    }


type alias Props =
    { token : String
    , tokenSubmitButtonText : String
    , tokenSubmittable : Bool
    , currentState : CurrentState
    }


render : Effects msg -> Props -> Html msg
render eff props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        ]
        [ currentState eff props
        , tokenForm eff props
        ]


tokenForm : Effects msg -> Props -> Html msg
tokenForm eff props =
    let
        tokenInputId =
            "discordTokenInput"
    in
    div [ flexColumn, spacingColumn2 ]
        [ label [ flexItem, sizeTitle, bold, for tokenInputId ] [ t "Token" ]
        , p [ colorNote ] [ t "Some shady works required to acquire Discord personal access token. Do not talk about it." ]
        , p [ colorNote ]
            [ t "Tokens are stored in IndexedDB of your web browser, and only sent to 'discordapp.com'. Otherwise it "
            , strong [ bold ] [ t "never" ]
            , t " get out of your web browser."
            ]
        , input
            [ type_ "text"
            , value props.token
            , id tokenInputId
            , onInput eff.onTokenInput
            , flexItem
            , sizeHeadline
            , padding5
            , Border.round5
            ]
            []
        , button
            [ class tokenSubmitButtonClass
            , flexItem
            , sizeHeadline
            , padding10
            , Background.colorPrim
            , disabled (not props.tokenSubmittable)
            , onClick eff.onTokenSubmit
            ]
            [ t props.tokenSubmitButtonText ]
        ]


type CurrentState
    = NotIdentified
    | NowHydrating Discord.User
    | HydratedOnce Bool Discord.POV


currentState : Effects msg -> Props -> Html msg
currentState eff props =
    case props.currentState of
        NotIdentified ->
            none

        NowHydrating user ->
            div [] [ userNameAndAvatar eff.onRehydrateButtonClick False user ]

        HydratedOnce rehydrating pov ->
            div [ flexColumn, spacingColumn5 ]
                [ userNameAndAvatar eff.onRehydrateButtonClick rehydrating pov.user
                , guilds pov
                ]


userNameAndAvatar : msg -> Bool -> Discord.User -> Html msg
userNameAndAvatar onRehydrateButtonClick rehydrating user =
    div [ flexRow, spacingRow5 ]
        [ img
            [ class icon40Class
            , flexItem
            , src (Discord.imageUrlWithFallback (Just icon40Size) user.discriminator user.avatar)
            , alt user.username
            , Border.round5
            ]
            []
        , div [ flexGrow ]
            [ h3 [ sizeHeadline, bold ] [ t user.username ]
            , p [ colorNote ] [ t ("#" ++ user.discriminator) ]
            ]
        , rehydrateButton onRehydrateButtonClick rehydrating
        ]


rehydrateButton : msg -> Bool -> Html msg
rehydrateButton onRehydrateButtonClick rehydrating =
    Icon.octiconButton
        [ class rehydrateButtonClass
        , disabled rehydrating
        , Border.elliptic
        , Background.transparent
        , if rehydrating then
            Animation.rotating

          else
            noAttr
        ]
        { onPress = onRehydrateButtonClick
        , size = rehydrateButtonSize
        , shape = Octicons.sync
        }


rehydrateButtonSize : Int
rehydrateButtonSize =
    20


guilds : Discord.POV -> Html msg
guilds pov =
    Html.Keyed.node "div" [ flexRow, flexWrap, spacingWrapped5 ] <|
        if Dict.isEmpty pov.guilds then
            [ ( "discordGuildEmpty", p [ colorNote, flexGrow ] [ t "(No Servers)" ] ) ]

        else
            Dict.foldr (\_ g a -> guildIconKey g :: a) [] pov.guilds


guildIconKey : Discord.Guild -> ( String, Html msg )
guildIconKey g =
    Tuple.pair g.id <|
        case Maybe.map (Discord.imageUrlNoFallback (Just icon40Size)) g.icon of
            Just src_ ->
                img [ class icon40Class, Border.round5, src src_, alt g.name ] []

            Nothing ->
                Icon.abbr [ class icon40Class, Border.round5, serif, sizeTitle ] g.name



-- STYLES


styles : List Style
styles =
    [ s (c tokenSubmitButtonClass) [ ( "align-self", "flex-end" ) ]
    , s (c icon40Class) [ ( "width", px icon40Size ), ( "height", px icon40Size ), ( "flex-basis", "auto" ) ]
    , s (c rehydrateButtonClass) [ ( "align-self", "flex-start" ) ]
    , octiconPathStyle (c rehydrateButtonClass) [ ( "fill", cssRgba oneDarkTheme.prim ) ]
    ]


tokenSubmitButtonClass : String
tokenSubmitButtonClass =
    "discordtokenbtn"


icon40Class : String
icon40Class =
    "discordicon40"


icon40Size : Int
icon40Size =
    40


rehydrateButtonClass : String
rehydrateButtonClass =
    "discordrehy"
