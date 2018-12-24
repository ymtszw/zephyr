module View.HtmlParts exposing
    ( alignTop
    , bdRounded
    , bgColor
    , bold
    , breakClassName
    , codeBlock
    , codeInline
    , column
    , downloadLink
    , extraBold
    , flex
    , fluidContainer
    , fontColor
    , fontSize
    , forceBreak
    , grow
    , gutterWidth
    , gutteredConteinerAttrs
    , iconWithBadge
    , indicator
    , inlinePadding
    , italic
    , maxHeight
    , maxWidth
    , monospace
    , newTabLink
    , none
    , noneAttr
    , octicon
    , padding
    , paddingXY
    , paragraph
    , px
    , row
    , sanSerif
    , scrollbarX
    , scrollbarY
    , squareIconOrHead
    , underline
    , widthFill
    )

import Data.ColorTheme exposing (ColorTheme)
import Element exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import Octicons
import View.Parts exposing (cssRgba, rectElementInnerPadding, rectElementRound)


none : Html msg
none =
    text ""


flex : Attribute msg
flex =
    style "flex" "1 0"


grow : Attribute msg
grow =
    style "flex-grow" "10000"


fluidContainer : Attribute msg
fluidContainer =
    class fluidContainerClassName


fluidContainerClassName : String
fluidContainerClassName =
    "fluidContainer"


paragraph : List (Attribute msg) -> List (Html msg) -> Html msg
paragraph attrs children =
    case children of
        [] ->
            none

        _ ->
            let
                baseAttrs =
                    [ forceBreak
                    , style "line-height" "1.3em"
                    ]
            in
            p (baseAttrs ++ attrs) children


gutteredConteinerAttrs : ColorTheme -> Maybe Color -> List (Attribute msg)
gutteredConteinerAttrs theme gutterColor =
    [ widthFill
    , padding rectElementInnerPadding
    , style "border-left" <|
        String.join " "
            [ px gutterWidth, "solid", cssRgba (Maybe.withDefault theme.bd gutterColor) ]
    , style "border-radius" (String.join " " [ px gutterWidth, "0", "0", px gutterWidth ])
    ]


gutterWidth : Int
gutterWidth =
    3


codeBlock :
    List (Attribute msg)
    -> { theme : ColorTheme, maxHeight : Int, code : String }
    -> Html msg
codeBlock attrs opts =
    let
        baseAttrs =
            [ widthFill
            , maxHeight opts.maxHeight
            , padding rectElementInnerPadding
            , scrollbarX
            , scrollbarY
            , bdRounded rectElementRound
            , bgColor opts.theme.sub
            , monospace
            ]
    in
    pre (baseAttrs ++ attrs) [ text opts.code ]


maxHeight : Int -> Attribute msg
maxHeight mh =
    style "max-height" (px mh)


px : Int -> String
px i =
    String.fromInt i ++ "px"


padding : Int -> Attribute msg
padding pa =
    style "padding" (px pa)


scrollbarX : Attribute msg
scrollbarX =
    style "overflow-x" "auto"


scrollbarY : Attribute msg
scrollbarY =
    style "overflow-y" "auto"


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    case children of
        [] ->
            none

        _ ->
            let
                flexAttrs =
                    [ style "display" "flex"
                    , style "flex-direction" "row"
                    , style "justify-content" "flex-start"
                    , style "box-sizing" "border-box"
                    ]
            in
            div (flexAttrs ++ attrs) children


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    case children of
        [] ->
            none

        _ ->
            let
                flexAttrs =
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "flex-start"
                    , style "box-sizing" "border-box"
                    ]
            in
            div (flexAttrs ++ attrs) children


widthFill : Attribute msg
widthFill =
    style "width" "100%"


octicon : List (Attribute msg) -> { size : Int, color : Color, shape : Octicons.Options -> Html msg } -> Html msg
octicon attrs opts =
    div attrs
        [ Octicons.defaultOptions
            |> Octicons.color (cssRgba opts.color)
            |> Octicons.size opts.size
            |> opts.shape
        ]


alignTop : Attribute msg
alignTop =
    style "align-self" "flex-start"


noneAttr : Attribute msg
noneAttr =
    property "none" Json.Encode.null


newTabLink : List (Attribute msg) -> { url : String, children : List (Html msg) } -> Html msg
newTabLink attrs opts =
    let
        linkAttrs =
            [ href opts.url
            , rel "noopener noreferrer"
            , target "_blank"
            ]
    in
    a (linkAttrs ++ attrs) opts.children


downloadLink : List (Attribute msg) -> { url : String, children : List (Html msg) } -> Html msg
downloadLink attrs opts =
    let
        linkAttrs =
            [ href opts.url
            , download ""
            ]
    in
    a (linkAttrs ++ attrs) opts.children


maxWidth : Int -> Attribute msg
maxWidth mh =
    style "max-width" (px mh)


italic : Attribute msg
italic =
    style "font-style" "italic"


bold : Attribute msg
bold =
    style "font-weight" "700"


extraBold : Attribute msg
extraBold =
    style "font-weight" "1000"


underline : Attribute msg
underline =
    style "text-decoration" "undeline"


codeInline : List (Attribute msg) -> { theme : ColorTheme, code : String } -> Html msg
codeInline attrs opts =
    let
        baseAttrs =
            [ paddingXY inlinePadding 0
            , bdRounded inlinePadding
            , bgColor opts.theme.text
            , fontColor opts.theme.err
            , monospace
            , forceBreak
            ]
    in
    span (baseAttrs ++ attrs) [ text opts.code ]


paddingXY : Int -> Int -> Attribute msg
paddingXY x y =
    style "padding" (String.join " " [ px y, px x, px y, px x ])


inlinePadding : Int
inlinePadding =
    2


bdRounded : Int -> Attribute msg
bdRounded r =
    style "border-radius" (px r)


fontColor : Color -> Attribute msg
fontColor color =
    style "color" (cssRgba color)


fontSize : Int -> Attribute msg
fontSize size =
    style "font-size" (px size)


bgColor : Color -> Attribute msg
bgColor color =
    style "background-color" (cssRgba color)


monospace : Attribute msg
monospace =
    style "font-family" "Lucida Console,Monaco,monospace"


forceBreak : Attribute msg
forceBreak =
    class breakClassName


breakClassName : String
breakClassName =
    "breakEl"


squareIconOrHead : List (Attribute msg) -> { theme : ColorTheme, size : Int, name : String, url : Maybe String } -> Html msg
squareIconOrHead userAttrs { theme, size, name, url } =
    case url of
        Just url_ ->
            let
                imgAttrs =
                    [ width size
                    , height size
                    , src url_
                    , alt name
                    , bdRounded (iconRounding size)
                    , style "overflow" "hidden"
                    ]
            in
            img (imgAttrs ++ userAttrs) []

        Nothing ->
            let
                baseAttrs =
                    [ style "width" (px size)
                    , style "height" (px size)
                    , fontSize (size // 2)
                    , bdRounded (iconRounding size)
                    , bgColor theme.bg
                    , bold
                    , sanSerif
                    , style "display" "flex"
                    , style "align-items" "center"
                    , style "justify-content" "center"
                    ]
            in
            div (baseAttrs ++ userAttrs) [ text (String.left 1 name) ]


iconRounding : Int -> Int
iconRounding size =
    Basics.max 2 (size // 10)


sanSerif : Attribute msg
sanSerif =
    style "font-family" <|
        String.join ","
            [ "Tahoma"
            , "Verdana"
            , "Arial"
            , "Helvetica"
            , "sans-serif"
            ]


iconWithBadge :
    List (Attribute msg)
    ->
        { size : Int
        , theme : ColorTheme
        , badge : Maybe (Int -> Html msg)
        , fallback : String
        , url : Maybe String
        }
    -> Html msg
iconWithBadge userAttrs opts =
    let
        outerAttrs =
            [ flex
            , style "display" "flex"
            , style "flex-direction" "row-reverse" -- HACK!
            , alignTop
            ]

        innerIconPadding =
            Basics.max 1 (opts.size // 20)
    in
    div (outerAttrs ++ userAttrs)
        [ div [ padding innerIconPadding ]
            [ squareIconOrHead
                [ case opts.url of
                    Just _ ->
                        noneAttr

                    Nothing ->
                        bgColor opts.theme.prim
                ]
                { theme = opts.theme
                , size = opts.size - (innerIconPadding * 2)
                , name = opts.fallback
                , url = opts.url
                }
            ]
        , case opts.badge of
            Just badge ->
                let
                    badgeSize =
                        opts.size // 3

                    badgePadding =
                        opts.size - badgeSize
                in
                div
                    [ bdRounded (iconRounding badgeSize)
                    , style "align-self" "flex-end"
                    , style "position" "absolute"
                    , style "overflow" "hidden"
                    ]
                    [ badge badgeSize ]

            Nothing ->
                none
        ]


indicator : String -> Attribute msg
indicator d =
    attribute "data-indicator" d
