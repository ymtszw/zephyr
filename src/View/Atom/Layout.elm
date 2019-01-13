module View.Atom.Layout exposing
    ( widthFill, block
    , flexRow, growRow, flexColumn, growColumn, flexItem, growItem, flexGrow, flexShrink, flexCenter, flexBasis, flexBasisAuto
    , noPadding, padding2, padding5, padding10, padding15, paddingInline
    , spacingRow2, spacingRow5, spacingRow10, spacingRow15
    , spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
    , withBadge
    , styles, paddingInlineStyle
    )

{-| Essential layouting Atoms.

@docs widthFill, block
@docs flexRow, growRow, flexColumn, growColumn, flexItem, growItem, flexGrow, flexShrink, flexCenter, flexBasis, flexBasisAuto
@docs noPadding, padding2, padding5, padding10, padding15, paddingInline
@docs spacingRow2, spacingRow5, spacingRow10, spacingRow15
@docs spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
@docs withBadge
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


{-| Styles equivalent to this class are automatically applied to direct children of `flexRow` or `flexColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>` or `<blockquote>`.
-}
flexItem : Attribute msg
flexItem =
    class flexItemClass


{-| Styles equivalent to this class are automatically applied to direct children of `growRow` or `growColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>` or `<blockquote>`.
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



-- STYLE


styles : List Style
styles =
    -- XXX Order matters!
    [ s (c widthFillClass) [ ( "width", "100%" ) ]
    , s (c blockClass) [ ( "display", "block" ) ]
    , flexRowStyle
    , flexColumnStyle
    , derive ("." ++ growRowClass) flexRowStyle
    , derive ("." ++ growColumnClass) flexColumnStyle
    , autoFlexItemStyle
    , autoGrowItemStyle
    , flexItemStyle
    , growItemStyle
    , flexGrowStyle
    , flexShrinkStyle
    , flexCenterStyle
    , s (c flexBasisAutoClass) [ ( "flex-basis", "auto" ) ]
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
    [ "div", "pre", "p", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote" ]


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
        growingChildlen =
            String.join ","
                [ c flexRowClass ++ ">" ++ c flexGrowClass
                , c flexColumnClass ++ ">" ++ c flexGrowClass
                , c growRowClass ++ ">" ++ c flexGrowClass
                , c growColumnClass ++ ">" ++ c flexGrowClass
                ]
    in
    s growingChildlen [ ( "flex-grow", "10000" ) ]


flexGrowClass : String
flexGrowClass =
    "flg"


flexShrinkStyle : Style
flexShrinkStyle =
    let
        shrinkingChildlen =
            String.join ","
                [ c flexRowClass ++ ">" ++ c flexShrinkClass
                , c flexColumnClass ++ ">" ++ c flexShrinkClass
                , c growRowClass ++ ">" ++ c flexShrinkClass
                , c growColumnClass ++ ">" ++ c flexShrinkClass
                ]
    in
    s shrinkingChildlen [ ( "flex-shrink", "1" ) ]


flexShrinkClass : String
flexShrinkClass =
    "fls"


flexCenterStyle : Style
flexCenterStyle =
    let
        shrinkingChildlen =
            String.join ","
                [ c flexRowClass ++ c flexCenterClass
                , c flexColumnClass ++ c flexCenterClass
                , c growRowClass ++ c flexCenterClass
                , c growColumnClass ++ c flexCenterClass
                ]
    in
    s shrinkingChildlen [ ( "align-items", "center" ) ]


flexCenterClass : String
flexCenterClass =
    "flcenter"


flexBasisAutoClass : String
flexBasisAutoClass =
    "flbauto"


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


badgeStyles : List Style
badgeStyles =
    [ s (c badgeOuterClass)
        [ ( "display", "flex" )
        , ( "flex-direction", "row-reverse" )
        ]
    , s (c badgeTopRightClass)
        [ ( "position", "absolute" )
        , ( "align-self", "flex-start" )
        , ( "overflow", "hidden" )
        ]
    , s (c badgeBottomRightClass)
        [ ( "position", "absolute" )
        , ( "align-self", "flex-end" )
        , ( "overflow", "hidden" )
        ]
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
