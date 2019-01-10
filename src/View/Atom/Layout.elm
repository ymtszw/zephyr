module View.Atom.Layout exposing
    ( widthFill, flexRow, growRow, flexColumn, growColumn, flexItem, growItem, flexGrow, flexShrink, flexCenter, flexBasis
    , padding2, padding5, padding10, padding15, paddingInline
    , spacingRow2, spacingRow5, spacingRow10, spacingRow15
    , spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
    , styles, paddingInlineStyle
    )

{-| Essential layouting Atoms.

@docs widthFill, flexRow, growRow, flexColumn, growColumn, flexItem, growItem, flexGrow, flexShrink, flexCenter, flexBasis
@docs padding2, padding5, padding10, padding15, paddingInline
@docs spacingRow2, spacingRow5, spacingRow10, spacingRow15
@docs spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
@docs styles, paddingInlineStyle

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes
import View.Style exposing (..)


widthFill : Attribute msg
widthFill =
    Attributes.class widthFillClass


flexRow : Attribute msg
flexRow =
    Attributes.class flexRowClass


flexColumn : Attribute msg
flexColumn =
    Attributes.class flexColumnClass


{-| Mostly equivalent to `flexRow`,
with only difference is its child elements have `flex-grow: 1;` by default.

If you just want to have a flex row with growing children,
using this instead of `flexRow` may help.

-}
growRow : Attribute msg
growRow =
    Attributes.class growRowClass


{-| Similar to `growRow`, for a column.
-}
growColumn : Attribute msg
growColumn =
    Attributes.class growColumnClass


{-| Styles equivalent to this class are automatically applied to direct children of `flexRow` or `flexColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>` or `<blockquote>`.
-}
flexItem : Attribute msg
flexItem =
    Attributes.class flexItemClass


{-| Styles equivalent to this class are automatically applied to direct children of `growRow` or `growColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>` or `<blockquote>`.
-}
growItem : Attribute msg
growItem =
    Attributes.class growItemClass


flexGrow : Attribute msg
flexGrow =
    Attributes.class flexGrowClass


flexShrink : Attribute msg
flexShrink =
    Attributes.class flexShrinkClass


{-| Sets `align-items: center;`.

Basically, if paired with `flexRow`, it aligns its items vertically centered.
With `flexColumn`, aligns horizontally.

Not that it ceases to "stretch" children's cross-sizes. See
<https://developer.mozilla.org/ja/docs/Web/CSS/align-items>

-}
flexCenter : Attribute msg
flexCenter =
    Attributes.class flexCenterClass


{-| Flex elements such as `<img>`s may unintendedly collapse
if their contents are not loaded before reflow.
Explicitly setting this property can prevent that.

In `flexRow`, you should supply width value,
conversely in `flexColumn`, supply height value.

-}
flexBasis : String -> Attribute msg
flexBasis widthOrHeight =
    Attributes.style "flex-basis" widthOrHeight


padding2 : Attribute msg
padding2 =
    Attributes.class (paddingClass 2)


padding5 : Attribute msg
padding5 =
    Attributes.class (paddingClass 5)


padding10 : Attribute msg
padding10 =
    Attributes.class (paddingClass 10)


padding15 : Attribute msg
padding15 =
    Attributes.class (paddingClass 15)


paddingInline : Attribute msg
paddingInline =
    Attributes.class paddingInlineClass


spacingRow2 : Attribute msg
spacingRow2 =
    Attributes.class (spacingRowClass 2)


spacingRow5 : Attribute msg
spacingRow5 =
    Attributes.class (spacingRowClass 5)


spacingRow10 : Attribute msg
spacingRow10 =
    Attributes.class (spacingRowClass 10)


spacingRow15 : Attribute msg
spacingRow15 =
    Attributes.class (spacingRowClass 15)


spacingColumn2 : Attribute msg
spacingColumn2 =
    Attributes.class (spacingColumnClass 2)


spacingColumn5 : Attribute msg
spacingColumn5 =
    Attributes.class (spacingColumnClass 5)


spacingColumn10 : Attribute msg
spacingColumn10 =
    Attributes.class (spacingColumnClass 10)


spacingColumn15 : Attribute msg
spacingColumn15 =
    Attributes.class (spacingColumnClass 15)


styles : List Style
styles =
    -- XXX Order matters!
    [ c widthFillClass [ ( "width", "100%" ) ]
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


widthFillClass : String
widthFillClass =
    "wf"


flexRowStyle : Style
flexRowStyle =
    c flexRowClass [ ( "display", "flex" ), ( "flex-direction", "row" ) ]


flexRowClass : String
flexRowClass =
    "flr"


flexColumnStyle : Style
flexColumnStyle =
    c flexColumnClass [ ( "display", "flex" ), ( "flex-direction", "column" ) ]


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
            [ "." ++ flexRowClass ++ ">" ++ tag
            , "." ++ flexColumnClass ++ ">" ++ tag
            ]
    in
    derive autoFlexItemSelector flexItemStyle


autoFlexItemSelectors : List String
autoFlexItemSelectors =
    [ "div", "pre", "p", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote" ]


flexItemStyle : Style
flexItemStyle =
    c flexItemClass
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
            [ "." ++ growRowClass ++ ">" ++ tag
            , "." ++ growColumnClass ++ ">" ++ tag
            ]
    in
    derive autoGrowItemSelector growItemStyle


growItemStyle : Style
growItemStyle =
    c growItemClass
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
                [ "." ++ flexRowClass ++ ">." ++ flexGrowClass
                , "." ++ flexColumnClass ++ ">." ++ flexGrowClass
                , "." ++ growRowClass ++ ">." ++ flexGrowClass
                , "." ++ growColumnClass ++ ">." ++ flexGrowClass
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
                [ "." ++ flexRowClass ++ ">." ++ flexShrinkClass
                , "." ++ flexColumnClass ++ ">." ++ flexShrinkClass
                , "." ++ growRowClass ++ ">." ++ flexShrinkClass
                , "." ++ growColumnClass ++ ">." ++ flexShrinkClass
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
                [ "." ++ flexRowClass ++ "." ++ flexCenterClass
                , "." ++ flexColumnClass ++ "." ++ flexCenterClass
                , "." ++ growRowClass ++ "." ++ flexCenterClass
                , "." ++ growColumnClass ++ "." ++ flexCenterClass
                ]
    in
    s shrinkingChildlen [ ( "align-items", "center" ) ]


flexCenterClass : String
flexCenterClass =
    "flcenter"


paddingStyle : Int -> Style
paddingStyle pad =
    c (paddingClass pad) [ ( "padding", px pad ) ]


paddingClass : Int -> String
paddingClass pad =
    "pa" ++ String.fromInt pad


paddingInlineStyle : Style
paddingInlineStyle =
    c paddingInlineClass
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
            "." ++ spacingRowClass space ++ ">" ++ selector ++ ":nth-child(n+2)"
    in
    s spacedItemsSelector [ ( "margin-left", px space ) ]


flexItems : List String
flexItems =
    ("." ++ flexItemClass) :: ("." ++ growItemClass) :: autoFlexItemSelectors


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
            "." ++ spacingColumnClass space ++ ">" ++ selector ++ ":nth-child(n+2)"
    in
    s spacedItemsSelector [ ( "margin-top", px space ) ]


spacingColumnClass : Int -> String
spacingColumnClass space =
    "spc" ++ String.fromInt space
