module View.Organism.Config.Discord exposing (CurrentState(..), Effects, Props, render, styles)

import Color exposing (cssRgba)
import Data.Producer.Discord as Discord
import Html exposing (Html, button, div, h3, img, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Octicons
import View.Atom.Animation as Animation
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image exposing (octiconPathStyle)
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon exposing (octiconButton)
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
                ]


userNameAndAvatar : msg -> Bool -> Discord.User -> Html msg
userNameAndAvatar onRehydrateButtonClick rehydrating user =
    div [ flexRow, spacingRow5 ]
        [ img
            [ class userAvatarClass
            , flexItem
            , src (Discord.imageUrlWithFallback (Just userAvatarSize) user.discriminator user.avatar)
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
    octiconButton
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



-- STYLES


styles : List Style
styles =
    [ s (c tokenSubmitButtonClass) [ ( "align-self", "flex-end" ) ]
    , s (c userAvatarClass) [ ( "width", px userAvatarSize ), ( "height", px userAvatarSize ) ]
    , s (c rehydrateButtonClass) [ ( "align-self", "flex-start" ) ]
    , octiconPathStyle (c rehydrateButtonClass) [ ( "fill", cssRgba oneDarkTheme.prim ) ]
    ]


tokenSubmitButtonClass : String
tokenSubmitButtonClass =
    "discordtokenbtn"


userAvatarClass : String
userAvatarClass =
    "discordavatar"


userAvatarSize : Int
userAvatarSize =
    40


rehydrateButtonClass : String
rehydrateButtonClass =
    "discordrehy"
