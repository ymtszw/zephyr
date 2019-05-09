module View.Molecules.MediaViewer exposing (Effects, Media(..), Props, render, styles)

import Color exposing (toCssString)
import ColorExtra
import Html exposing (Html, button, div, img, video)
import Html.Attributes exposing (class, controls, src, style)
import Html.Events exposing (onClick)
import Octicons
import SelectList exposing (SelectList)
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
    }


type alias Props =
    { selectedMedia : Media
    , hasMore : Bool
    }


{-| Possibly add external services for embedded players like YouTube or Twitch
-}
type Media
    = Image String
    | Video String


render : Effects msg -> Props -> Html msg
render eff props =
    div [ class mediaViewerClass, flexRow, flexCenter, flexBasisAuto, Background.colorBg ] <|
        case props.selectedMedia of
            Image src_ ->
                [ img [ flexItem, flexBasisAuto, flexShrink, src src_ ] []
                , hoverMenu eff.onPagerClick props.hasMore src_
                ]

            Video src_ ->
                [ hoverMenu eff.onPagerClick props.hasMore src_
                , -- Video has clickable control, thus it must come ABOVE hoverMenu
                  video [ flexItem, flexBasisAuto, flexShrink, src src_, controls True ]
                    [ t "Embedded video not supported. "
                    , ntLink [] { url = src_, children = [ t "[Source]" ] }
                    ]
                ]


hoverMenu : (Int -> msg) -> Bool -> String -> Html msg
hoverMenu onPagerClick hasMore src_ =
    div [ class hoverMenuClass, widthFill, flexColumn, flexBasisAuto ]
        [ if hasMore then
            let
                pagerClickArea step pagerIcon =
                    div
                        [ class pagerClickAreaClass
                        , flexGrow
                        , flexRow
                        , flexCenter
                        , Image.hovText
                        , Cursor.pointer
                        , onClick (onPagerClick step)
                        ]
                        [ pagerIcon ]
            in
            div [ flexRow, flexGrow ]
                [ pagerClickArea -1 <|
                    div [ class pagerIconClass ]
                        [ Image.octicon { size = xProminentSize, shape = Octicons.chevronLeft } ]
                , div [ flexGrow ] [] -- Shim
                , pagerClickArea 1 <|
                    div [ class pagerIconClass, pushRight ]
                        [ Image.octicon { size = xProminentSize, shape = Octicons.chevronRight } ]
                ]

          else
            none
        , div [ class paletteClass, flexRow, widthFill ]
            [ div [ pushRight, padding10 ]
                [ ntLink []
                    { url = src_
                    , children =
                        [ div [ padding5, Border.round5, Background.colorSub, Image.hovText ]
                            [ Image.octicon { size = prominentSize, shape = Octicons.linkExternal } ]
                        ]
                    }
                ]
            ]
        ]



-- STYLE


styles : List Style
styles =
    [ s (c mediaViewerClass)
        [ -- Default size; fixed by viewport-ratio
          ( "width", "80vw" )
        , ( "height", "80vh" )
        , ( "position", "relative" )
        ]
    , s (descOf (c mediaViewerClass) "img," ++ descOf (c mediaViewerClass) "video")
        [ ( "max-width", "95%" )
        , ( "max-height", "95%" )
        , ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        ]
    , s (c hoverMenuClass)
        [ ( "position", "absolute" )
        , ( "display", "none" )
        , ( "height", "100%" )
        ]
    , s (descOf (hov (c mediaViewerClass)) (c hoverMenuClass)) [ ( "display", "flex" ) ]
    , s (c pagerIconClass) [ ( "opacity", "0" ) ]
    , s (hov (c pagerClickAreaClass)) [ ( "background-color", toCssString (ColorExtra.setAlpha 0.3 oneDarkTheme.bg) ) ]
    , s (descOf (hov (c pagerClickAreaClass)) (c pagerIconClass)) [ ( "opacity", "1" ) ]
    , s (c paletteClass) [ ( "position", "absolute" ) ]
    ]


mediaViewerClass : String
mediaViewerClass =
    "mv"


hoverMenuClass : String
hoverMenuClass =
    "mvhm"


pagerClickAreaClass : String
pagerClickAreaClass =
    "mvhmpca"


pagerIconClass : String
pagerIconClass =
    "mvhmpi"


paletteClass : String
paletteClass =
    "mvhmpa"
