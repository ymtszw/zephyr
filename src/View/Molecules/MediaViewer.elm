module View.Molecules.MediaViewer exposing (Effects, Media(..), Props, render, styles)

import Color exposing (toCssString)
import ColorExtra
import Html exposing (Html, button, div, iframe, img, span, video)
import Html.Attributes exposing (attribute, autoplay, class, controls, height, src, type_, width)
import Html.Events exposing (stopPropagationOn)
import Json.Decode exposing (succeed)
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Theme exposing (oneDarkTheme)
import View.Atoms.Typography exposing (..)
import View.Style exposing (..)


type alias Effects msg =
    { onPagerClick : Int -> msg
    , onToggleSizeClick : Bool -> msg
    }


type alias Props =
    { selectedMedia : Media
    , nMedia : Int
    , mediaIndex : Int
    , isShrunk : Bool
    }


type Media
    = Image String
    | Video String
    | Youtube String
      -- TODO: Youtube playlist
    | TwitchChannel String
    | TwitchClip String
      -- TODO: Twitch video/collection
    | NotFound


render : Effects msg -> Props -> Html msg
render eff props =
    div
        [ class mediaViewerClass
        , if props.isShrunk || props.selectedMedia == NotFound then
            class shrunkClass

          else
            noAttr
        , flexRow
        , flexCenter
        , flexBasisAuto
        , Background.colorBg
        ]
    <|
        case props.selectedMedia of
            Image src_ ->
                [ img [ flexItem, flexBasisAuto, flexShrink, src src_ ] []
                , pager eff props
                , menu eff props src_
                ]

            Video src_ ->
                [ -- Video has clickable control, thus it must come ABOVE pager
                  pager eff props
                , video [ flexItem, flexBasisAuto, flexShrink, src src_, controls True, autoplay True ]
                    [ t "Embedded video not supported. "
                    , ntLink [] { url = src_, children = [ t "[Source]" ] }
                    ]
                , menu eff props src_
                ]

            Youtube id ->
                [ pager eff props
                , iframe
                    [ flexItem
                    , flexBasisAuto
                    , flexShrink
                    , type_ "text/html"
                    , -- Forced standard size. Users may choose fullscreen if they want.
                      width 640
                    , height 360
                    , src ("http://www.youtube.com/embed/" ++ id ++ "?autoplay=1")
                    , attribute "frameborder" "0"
                    , attribute "allowfullscreen" ""
                    ]
                    []
                , menu eff props ("https://www.youtube.com/watch?v=" ++ id)
                ]

            TwitchChannel id ->
                [ pager eff props
                , iframe
                    [ flexItem
                    , flexBasisAuto
                    , flexShrink
                    , type_ "text/html"
                    , -- Forced standard size. Users may choose fullscreen if they want.
                      width 640
                    , height 360
                    , src ("https://player.twitch.tv/?channel=" ++ id ++ "?autoplay=true")
                    , attribute "frameborder" "0"
                    , attribute "allowfullscreen" "true"
                    ]
                    []
                , menu eff props ("https://www.twitch.tv/" ++ id)
                ]

            TwitchClip slug ->
                [ pager eff props
                , iframe
                    [ flexItem
                    , flexBasisAuto
                    , flexShrink
                    , type_ "text/html"
                    , -- Forced standard size. Users may choose fullscreen if they want.
                      width 640
                    , height 360
                    , src ("https://clips.twitch.tv/embed?clip=" ++ slug)
                    , attribute "frameborder" "0"
                    , attribute "allowfullscreen" "true"
                    ]
                    []
                , menu eff props ("https://clips.twitch.tv/" ++ slug)
                ]

            NotFound ->
                [ div [ widthFill, flexColumn, flexCenter, flexBasisAuto ]
                    [ Image.octicon { size = xxxProminentSize, shape = Octicons.alert }
                    , span [ xxProminent, colorNote ] [ t "Media Not Found" ]
                    ]
                ]


menu : Effects msg -> Props -> String -> Html msg
menu eff props src_ =
    div [ class menuClass, flexRow, widthFill, alignStart ]
        [ div [ pushRight, flexRow, spacingRow10, padding10 ]
            [ button
                [ padding5
                , Border.round5
                , Background.colorSub
                , Image.hovText
                , stopPropagationOn "click" (succeed ( eff.onToggleSizeClick (not props.isShrunk), True ))
                ]
                [ Image.octicon
                    { size = prominentSize
                    , shape =
                        if props.isShrunk then
                            Octicons.screenFull

                        else
                            Octicons.screenNormal
                    }
                ]
            , ntLink [ flexItem ]
                { url = src_
                , children =
                    [ div [ padding5, Border.round5, Background.colorSub, Image.hovText ]
                        [ Image.octicon { size = prominentSize, shape = Octicons.linkExternal } ]
                    ]
                }
            ]
        ]


pager : Effects msg -> Props -> Html msg
pager eff props =
    if props.nMedia > 1 then
        let
            pagerClickArea indexTo pagerIcon =
                div
                    [ class pagerClickAreaClass
                    , flexBasisAuto
                    , flexRow
                    , flexCenter
                    , Image.hovText
                    , Cursor.pointer
                    , stopPropagationOn "click" (succeed ( eff.onPagerClick indexTo, True ))
                    ]
                    [ pagerIcon ]

            prevIndex =
                if props.mediaIndex > 0 then
                    props.mediaIndex - 1

                else
                    props.nMedia - 1

            nextIndex =
                if props.mediaIndex + 1 < props.nMedia then
                    props.mediaIndex + 1

                else
                    0
        in
        div [ class pagerClass, flexRow, flexGrow, widthFill ]
            [ pagerClickArea prevIndex <|
                div [ class pagerIconClass ]
                    [ Image.octicon { size = xProminentSize, shape = Octicons.chevronLeft } ]
            , div [ flexGrow ] [] -- Shim
            , pagerClickArea nextIndex <|
                div [ class pagerIconClass, pushRight ]
                    [ Image.octicon { size = xProminentSize, shape = Octicons.chevronRight } ]
            ]

    else
        none



-- STYLE


styles : List Style
styles =
    [ s (c mediaViewerClass)
        [ -- Default size; fixed by viewport-ratio
          ( "width", "80vw" )
        , ( "height", "80vh" )
        , ( "position", "relative" )
        ]
    , s (c shrunkClass)
        [ ( "width", "30vw" )
        , ( "height", "30vh" )
        ]
    , let
        selector =
            [ "img", "video", "iframe" ]
                |> List.map (descOf (c mediaViewerClass))
                |> String.join ","
      in
      s selector
        [ ( "max-width", "95%" )
        , ( "max-height", "95%" )
        , ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        , ( "position", "relative" )
        ]
    , s (c pagerClass)
        [ ( "position", "absolute" )
        , ( "display", "none" )
        , ( "height", "100%" )
        , ( "pointer-events", "none" )
        ]
    , s (descOf (hov (c mediaViewerClass)) (c pagerClass)) [ ( "display", "flex" ) ]
    , s (c menuClass) [ ( "position", "absolute" ), ( "display", "none" ) ]
    , s (descOf (hov (c mediaViewerClass)) (c menuClass)) [ ( "display", "flex" ) ]
    , s (c pagerIconClass) [ ( "opacity", "0" ) ]
    , s (c pagerClickAreaClass) [ ( "width", "30%" ), ( "pointer-events", "auto" ) ]
    , s (hov (c pagerClickAreaClass)) [ ( "background-color", toCssString (ColorExtra.setAlpha 0.3 oneDarkTheme.bg) ) ]
    , s (descOf (hov (c pagerClickAreaClass)) (c pagerIconClass)) [ ( "opacity", "1" ) ]
    ]


mediaViewerClass : String
mediaViewerClass =
    "mv"


shrunkClass : String
shrunkClass =
    "mvs"


menuClass : String
menuClass =
    "mvm"


pagerClickAreaClass : String
pagerClickAreaClass =
    "mvpca"


pagerClass : String
pagerClass =
    "mvp"


pagerIconClass : String
pagerIconClass =
    "mvpi"
