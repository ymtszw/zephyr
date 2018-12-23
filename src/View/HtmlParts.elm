module View.HtmlParts exposing
    ( alignTop
    , bdRounded
    , bgColor
    , bold
    , breakClassName
    , codeBlock
    , codeInline
    , column
    , extraBold
    , fontColor
    , fontSize
    , forceBreak
    , gutterWidth
    , gutteredConteinerAttrs
    , inlinePadding
    , italic
    , maxHeight
    , maxWidth
    , monospace
    , newTabLink
    , noneAttr
    , octicon
    , padding
    , paddingXY
    , paragraph
    , px
    , row
    , scrollbarY
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


paragraph : List (Attribute msg) -> List (Html msg) -> Html msg
paragraph attrs children =
    let
        baseAttrs =
            [ forceBreak
            , style "line-height" "1.3em"
            , style "margin" "0"
            ]
    in
    p (baseAttrs ++ attrs) children


gutteredConteinerAttrs : ColorTheme -> Maybe Color -> List (Attribute msg)
gutteredConteinerAttrs theme gutterColor =
    [ widthFill
    , padding rectElementInnerPadding
    , style "border-left" (cssRgba (Maybe.withDefault theme.bd gutterColor) ++ " " ++ px gutterWidth)
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
            , scrollbarY
            , bdRounded rectElementRound
            , bgColor opts.theme.sub
            , monospace
            , style "width" "100%"
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


scrollbarY : Attribute msg
scrollbarY =
    style "overflow-y" "auto"


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "flex-start"
        ]
        children


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "flex-start"
        ]
        children


widthFill : Attribute msg
widthFill =
    -- style "width" "100%"
    noneAttr


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


maxWidth : Int -> Attribute msg
maxWidth mh =
    style "max-width" (px mh ++ "px")


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
