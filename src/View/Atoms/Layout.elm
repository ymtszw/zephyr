module View.Atoms.Layout exposing
    ( widthFill, block
    , flexRow, growRow, flexColumn, growColumn, flexWrap
    , flexItem, growItem, flexGrow, flexShrink, flexCenter, flexBasis, flexBasisAuto
    , alignEnd, alignStart, pushRight
    , noPadding, padding2, padding5, padding10, padding15, paddingInline
    , spacingRow2, spacingRow5, spacingRow10, spacingRow15
    , spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
    , spacingWrapped5, spacingWrapped10
    , withBadge, badgeOutset
    , styles, paddingInlineStyle
    )

{-| Essential layouting Atoms.

@docs widthFill, block
@docs flexRow, growRow, flexColumn, growColumn, flexWrap
@docs flexItem, growItem, flexGrow, flexShrink, flexCenter, flexBasis, flexBasisAuto
@docs alignEnd, alignStart, pushRight
@docs noPadding, padding2, padding5, padding10, padding15, paddingInline
@docs spacingRow2, spacingRow5, spacingRow10, spacingRow15
@docs spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
@docs spacingWrapped5, spacingWrapped10
@docs withBadge, badgeOutset
@docs styles, paddingInlineStyle

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import View.Style exposing (..)


widthFill : Attribute msg
widthFill =
    class widthFillClass


block : Attribute msg
block =
    class blockClass


flexRow : Attribute msg
flexRow =
    class flexRowClass


flexColumn : Attribute msg
flexColumn =
    class flexColumnClass


{-| Mostly equivalent to `flexRow`,
with only difference is its child elements have `flex-grow: 1;` by default.

If you just want to have a flex row with growing children,
using this instead of `flexRow` may help.

-}
growRow : Attribute msg
growRow =
    class growRowClass


{-| Similar to `growRow`, for a column.
-}
growColumn : Attribute msg
growColumn =
    class growColumnClass


flexWrap : Attribute msg
flexWrap =
    class flexWrapClass


{-| Styles equivalent to this class are automatically applied to direct children of `flexRow` or `flexColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>`, `<blockquote>`, `<textarea>`, or `<table>`.
-}
flexItem : Attribute msg
flexItem =
    class flexItemClass


{-| Styles equivalent to this class are automatically applied to direct children of `growRow` or `growColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>`, `<blockquote>`, `<textarea>`, or `<table>`.
-}
growItem : Attribute msg
growItem =
    class growItemClass


flexGrow : Attribute msg
flexGrow =
    class flexGrowClass


flexShrink : Attribute msg
flexShrink =
    class flexShrinkClass


{-| Sets `align-items: center;`.

Basically, if paired with `flexRow`, it aligns its items vertically centered.
With `flexColumn`, aligns horizontally.

Not that it ceases to "stretch" children's cross-sizes. See
<https://developer.mozilla.org/ja/docs/Web/CSS/align-items>

-}
flexCenter : Attribute msg
flexCenter =
    class flexCenterClass


{-| We set `flex-basis: 0%;` by default, in order to allow inline elements to collapse.
Explicitly setting this can prevent unintended collapsing.

In `flexRow`, you should supply width value,
conversely in `flexColumn`, supply height value.

Note that `flex-basis` has precedence over `width` or `height`.

-}
flexBasis : String -> Attribute msg
flexBasis widthOrHeight =
    style "flex-basis" widthOrHeight


{-| Set `flex-basis: auto;` which is UA default.

With this setting, containers respect items' `width` or `height`.

-}
flexBasisAuto : Attribute msg
flexBasisAuto =
    class flexBasisAutoClass


alignStart : Attribute msg
alignStart =
    class alignStartClass


alignEnd : Attribute msg
alignEnd =
    class alignEndClass


{-| Push a flex item (and items appearing thereafter) to right, in `flex-direction: row` containers.

This applies `margin-left: auto` to the item.

<https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Box_Alignment/Box_Alignment_in_Flexbox#Alignment_and_auto_margins>

-}
pushRight : Attribute msg
pushRight =
    class pushRightClass


noPadding : Attribute msg
noPadding =
    class (paddingClass 0)


padding2 : Attribute msg
padding2 =
    class (paddingClass 2)


padding5 : Attribute msg
padding5 =
    class (paddingClass 5)


padding10 : Attribute msg
padding10 =
    class (paddingClass 10)


padding15 : Attribute msg
padding15 =
    class (paddingClass 15)


paddingInline : Attribute msg
paddingInline =
    class paddingInlineClass


spacingRow2 : Attribute msg
spacingRow2 =
    class (spacingRowClass 2)


spacingRow5 : Attribute msg
spacingRow5 =
    class (spacingRowClass 5)


spacingRow10 : Attribute msg
spacingRow10 =
    class (spacingRowClass 10)


spacingRow15 : Attribute msg
spacingRow15 =
    class (spacingRowClass 15)


spacingColumn2 : Attribute msg
spacingColumn2 =
    class (spacingColumnClass 2)


spacingColumn5 : Attribute msg
spacingColumn5 =
    class (spacingColumnClass 5)


spacingColumn10 : Attribute msg
spacingColumn10 =
    class (spacingColumnClass 10)


spacingColumn15 : Attribute msg
spacingColumn15 =
    class (spacingColumnClass 15)


spacingWrapped5 : Attribute msg
spacingWrapped5 =
    class (spacingWrappedClass 5)


spacingWrapped10 : Attribute msg
spacingWrapped10 =
    class (spacingWrappedClass 10)


withBadge :
    List (Attribute msg)
    ->
        { topRight : Maybe (Html msg)
        , bottomRight : Maybe (Html msg)
        , content : Html msg
        }
    -> Html msg
withBadge userAttrs opts =
    -- XXX: Supporting left-aligned badges within this scheme is not straightforward; not doing now
    div (class badgeOuterClass :: userAttrs)
        [ opts.content
        , case opts.topRight of
            Just b ->
                div [ class badgeTopRightClass ] [ b ]

            Nothing ->
                none
        , case opts.bottomRight of
            Just b ->
                div [ class badgeBottomRightClass ] [ b ]

            Nothing ->
                none
        ]


badgeOutset : Attribute msg
badgeOutset =
    class badgeOutsetClass



-- STYLE


styles : List Style
styles =
    -- XXX Order matters!
    [ s (c widthFillClass) [ ( "width", "100%" ) ]
    , s (c blockClass) [ ( "display", "block" ) ]
    , flexRowStyle
    , flexColumnStyle
    , derive (c growRowClass) flexRowStyle
    , derive (c growColumnClass) flexColumnStyle
    , s (c flexWrapClass) [ ( "flex-wrap", "wrap" ) ]
    , autoFlexItemStyle
    , autoGrowItemStyle
    , flexItemStyle
    , growItemStyle
    , flexGrowStyle
    , flexShrinkStyle
    , flexCenterStyle
    , flexBasisAutoStyle
    , s (c alignStartClass) [ ( "align-self", "flex-start" ) ]
    , s (c alignEndClass) [ ( "align-self", "flex-end" ) ]
    , s (c pushRightClass) [ ( "margin-left", "auto!important" ) ]
    , paddingStyle 0
    , paddingStyle 2
    , paddingStyle 5
    , paddingStyle 10
    , paddingStyle 15
    , paddingInlineStyle
    , spacingRowStyle 2
    , spacingRowStyle 5
    , spacingRowStyle 10
    , spacingRowStyle 15
    , spacingColumnStyle 2
    , spacingColumnStyle 5
    , spacingColumnStyle 10
    , spacingColumnStyle 15
    , spacingWrappedParentStyle 5
    , spacingWrappedChildrenStyle 5
    , spacingWrappedParentStyle 10
    , spacingWrappedChildrenStyle 10
    ]
        ++ badgeStyles


widthFillClass : String
widthFillClass =
    "wf"


blockClass : String
blockClass =
    "bl"


flexRowStyle : Style
flexRowStyle =
    s (c flexRowClass) [ ( "display", "flex" ), ( "flex-direction", "row" ) ]


flexRowClass : String
flexRowClass =
    "flr"


flexColumnStyle : Style
flexColumnStyle =
    s (c flexColumnClass) [ ( "display", "flex" ), ( "flex-direction", "column" ) ]


flexColumnClass : String
flexColumnClass =
    "flc"


growRowClass : String
growRowClass =
    "grr"


growColumnClass : String
growColumnClass =
    "grc"


flexWrapClass : String
flexWrapClass =
    "flw"


autoFlexItemStyle : Style
autoFlexItemStyle =
    let
        autoFlexItemSelector =
            String.join "," <|
                List.concatMap childOfFlexBox <|
                    autoFlexItemSelectors

        childOfFlexBox tag =
            [ c flexRowClass ++ ">" ++ tag
            , c flexColumnClass ++ ">" ++ tag
            ]
    in
    derive autoFlexItemSelector flexItemStyle


autoFlexItemSelectors : List String
autoFlexItemSelectors =
    [ "div", "pre", "p", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote", "table", "textarea", "ul", "ol" ]


flexItemStyle : Style
flexItemStyle =
    s (c flexItemClass)
        [ ( "flex-grow", "0" )
        , ( "flex-shrink", "0" ) -- No, do not shrink past contents' dimension by default
        , ( "flex-basis", "0%" ) -- Allow elements to "collapse"
        ]


flexItemClass : String
flexItemClass =
    "fli"


autoGrowItemStyle : Style
autoGrowItemStyle =
    let
        autoGrowItemSelector =
            String.join "," <|
                List.concatMap childOfFlexBox <|
                    autoFlexItemSelectors

        childOfFlexBox tag =
            [ c growRowClass ++ ">" ++ tag
            , c growColumnClass ++ ">" ++ tag
            ]
    in
    derive autoGrowItemSelector growItemStyle


growItemStyle : Style
growItemStyle =
    s (c growItemClass)
        [ ( "flex-grow", "1" )
        , ( "flex-shrink", "0" )
        , ( "flex-basis", "0%" )
        ]


growItemClass : String
growItemClass =
    "gri"


flexGrowStyle : Style
flexGrowStyle =
    let
        growingChildren =
            String.join "," <|
                List.map (\parent -> parent ++ ">" ++ c flexGrowClass) <|
                    [ c flexRowClass
                    , c flexColumnClass
                    , c growRowClass
                    , c growColumnClass
                    , c badgeOuterClass
                    ]
    in
    s growingChildren [ ( "flex-grow", "10000" ) ]


flexGrowClass : String
flexGrowClass =
    "flg"


flexShrinkStyle : Style
flexShrinkStyle =
    let
        shrinkingChildren =
            String.join "," <|
                List.map (\parent -> parent ++ ">" ++ c flexShrinkClass) <|
                    [ c flexRowClass
                    , c flexColumnClass
                    , c growRowClass
                    , c growColumnClass
                    , c badgeOuterClass
                    ]
    in
    s shrinkingChildren [ ( "flex-shrink", "1" ) ]


flexShrinkClass : String
flexShrinkClass =
    "fls"


flexCenterStyle : Style
flexCenterStyle =
    let
        centeredFlex =
            String.join ","
                [ c flexRowClass ++ c flexCenterClass
                , c flexColumnClass ++ c flexCenterClass
                , c growRowClass ++ c flexCenterClass
                , c growColumnClass ++ c flexCenterClass
                ]
    in
    s centeredFlex [ ( "align-items", "center" ) ]


flexCenterClass : String
flexCenterClass =
    "flcenter"


flexBasisAutoStyle : Style
flexBasisAutoStyle =
    let
        children =
            String.join ","
                [ c flexRowClass ++ ">" ++ c flexBasisAutoClass
                , c flexColumnClass ++ ">" ++ c flexBasisAutoClass
                , c growRowClass ++ ">" ++ c flexBasisAutoClass
                , c growColumnClass ++ ">" ++ c flexBasisAutoClass
                ]
    in
    s children [ ( "flex-basis", "auto" ) ]


flexBasisAutoClass : String
flexBasisAutoClass =
    "flbauto"


alignStartClass : String
alignStartClass =
    "alstart"


alignEndClass : String
alignEndClass =
    "alend"


pushRightClass : String
pushRightClass =
    "pushr"


paddingStyle : Int -> Style
paddingStyle pad =
    s (c (paddingClass pad)) [ ( "padding", px pad ) ]


paddingClass : Int -> String
paddingClass pad =
    "pa" ++ String.fromInt pad


paddingInlineStyle : Style
paddingInlineStyle =
    s (c paddingInlineClass)
        [ ( "padding-left", "0.4em" )
        , ( "padding-top", "0.1em" )
        , ( "padding-right", "0.4em" )
        , ( "padding-bottom", "0.1em" )
        ]


paddingInlineClass : String
paddingInlineClass =
    "pail"


spacingRowStyle : Int -> Style
spacingRowStyle space =
    let
        spacedItemsSelector =
            String.join "," <|
                List.map trailingChildOfRow <|
                    flexItems

        trailingChildOfRow selector =
            c (spacingRowClass space) ++ ">" ++ selector ++ ":nth-child(n+2)"
    in
    s spacedItemsSelector [ ( "margin-left", px space ) ]


flexItems : List String
flexItems =
    c flexItemClass :: c growItemClass :: autoFlexItemSelectors


spacingRowClass : Int -> String
spacingRowClass space =
    "spr" ++ String.fromInt space


spacingColumnStyle : Int -> Style
spacingColumnStyle space =
    let
        spacedItemsSelector =
            String.join "," <|
                List.map trailingChildOfColumn <|
                    flexItems

        trailingChildOfColumn selector =
            c (spacingColumnClass space) ++ ">" ++ selector ++ ":nth-child(n+2)"
    in
    s spacedItemsSelector [ ( "margin-top", px space ) ]


spacingColumnClass : Int -> String
spacingColumnClass space =
    "spc" ++ String.fromInt space


spacingWrappedParentStyle : Int -> Style
spacingWrappedParentStyle space =
    s (c (spacingWrappedClass space))
        [ -- A trick also used in elm-ui; currently wrapping flex items cannot have easy "gap" in both row and column directions.
          -- So, (1) have margins around items, and (2) compensate edge margins by translating container box
          ( "margin", "-" ++ halfPx space )
        ]


spacingWrappedClass : Int -> String
spacingWrappedClass space =
    "spw" ++ String.fromInt space


halfPx : Int -> String
halfPx space =
    String.fromFloat (toFloat space / 2) ++ "px"


spacingWrappedChildrenStyle : Int -> Style
spacingWrappedChildrenStyle space =
    let
        spacedItemsSelector =
            String.join "," <| List.map child <| flexItems

        child selector =
            c (spacingWrappedClass space) ++ ">" ++ selector
    in
    s spacedItemsSelector [ ( "margin", halfPx space ) ]


badgeStyles : List Style
badgeStyles =
    [ s (c badgeOuterClass)
        [ ( "display", "flex" )
        , ( "flex-direction", "row-reverse" )
        , -- This is crucial; since badge children are positioned `absolute`-ly,
          -- and absolutely-positioned elements are positioned relative to its "nearest positioned ancestor".
          -- Nearest positioned ancestors mean containing block elements
          -- that has `position` property other than `static` (default value).
          -- <https://developer.mozilla.org/en-US/docs/Web/CSS/position#Absolute_positioning>
          ( "position", "relative" )
        ]
    , s (c badgeTopRightClass)
        [ ( "position", "absolute" )
        , ( "align-self", "flex-start" )
        , ( "overflow", "hidden" )
        ]
    , s (descOf (c badgeOutsetClass) (c badgeTopRightClass))
        [ ( "transform", "translate(" ++ px badgeOutsetSize ++ "," ++ px (negate badgeOutsetSize) ++ ")" ) ]
    , s (c badgeBottomRightClass)
        [ ( "position", "absolute" )
        , ( "align-self", "flex-end" )
        , ( "overflow", "hidden" )
        ]
    , s (descOf (c badgeOutsetClass) (c badgeBottomRightClass))
        [ ( "transform", "translate(" ++ px badgeOutsetSize ++ "," ++ px badgeOutsetSize ++ ")" ) ]
    ]


badgeOuterClass : String
badgeOuterClass =
    "badgeouter"


badgeTopRightClass : String
badgeTopRightClass =
    "badgetopr"


badgeBottomRightClass : String
badgeBottomRightClass =
    "badgebotr"


badgeOutsetClass : String
badgeOutsetClass =
    "badgeoutset"


badgeOutsetSize : Int
badgeOutsetSize =
    2
