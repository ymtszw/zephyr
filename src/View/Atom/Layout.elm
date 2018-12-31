module View.Atom.Layout exposing
    ( widthFill, flexRow, flexColumn, flexItem, flexGrow, flexShrink, flexCenter
    , padding2, padding5, padding10, padding15
    , spacingRow2, spacingRow5, spacingRow10, spacingRow15
    , spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
    , styles
    )

{-| Essential layouting Atoms.

@docs widthFill, flexRow, flexColumn, flexItem, flexGrow, flexShrink, flexCenter
@docs padding2, padding5, padding10, padding15
@docs spacingRow2, spacingRow5, spacingRow10, spacingRow15
@docs spacingColumn2, spacingColumn5, spacingColumn10, spacingColumn15
@docs styles

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


{-| Styles equivalent to this class are automatically applied to direct children of `flexRow` or `flexColumn`
if they are either `<div>`,`<pre>`,`<p>`,`<h1>` to `<h6>`, `<button>`, `<input>` or `<blockquote>`.
-}
flexItem : Attribute msg
flexItem =
    Attributes.class flexItemClass


flexGrow : Attribute msg
flexGrow =
    Attributes.class flexGrowClass


flexShrink : Attribute msg
flexShrink =
    Attributes.class flexShrinkClass


flexCenter : Attribute msg
flexCenter =
    Attributes.class flexCenterClass


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
    , c flexRowClass [ ( "display", "flex" ), ( "flex-direction", "row" ) ]
    , c flexColumnClass [ ( "display", "flex" ), ( "flex-direction", "column" ) ]
    , autoFlexItemStyle
    , flexItemStyle
    , flexGrowStyle
    , flexShrinkStyle
    , flexCenterStyle
    , paddingStyle 2
    , paddingStyle 5
    , paddingStyle 10
    , paddingStyle 15
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


flexRowClass : String
flexRowClass =
    "fr"


flexColumnClass : String
flexColumnClass =
    "fc"


autoFlexItemStyle : Style
autoFlexItemStyle =
    let
        autoFlexItemSelector =
            String.join "," <|
                List.concatMap childOfFlexBox <|
                    autoFlexItemTags

        childOfFlexBox tag =
            [ "." ++ flexRowClass ++ ">" ++ tag
            , "." ++ flexColumnClass ++ ">" ++ tag
            ]
    in
    derive autoFlexItemSelector flexItemStyle


autoFlexItemTags : List String
autoFlexItemTags =
    [ "div", "pre", "p", "h1", "h2", "h3", "h4", "h5", "h6", "button", "input", "blockquote" ]


flexItemStyle : Style
flexItemStyle =
    c flexItemClass
        [ ( "flex-grow", "0" )
        , ( "flex-shrink", "0" ) -- No, do not shrink past contents' dimension by default
        , ( "flex-basis", "0%" )
        ]


flexItemClass : String
flexItemClass =
    "fi"


flexGrowStyle : Style
flexGrowStyle =
    let
        growingChildlen =
            String.join ","
                [ "." ++ flexRowClass ++ ">." ++ flexGrowClass
                , "." ++ flexColumnClass ++ ">." ++ flexGrowClass
                ]
    in
    s growingChildlen [ ( "flex-grow", "10000" ) ]


flexGrowClass : String
flexGrowClass =
    "fg"


flexShrinkStyle : Style
flexShrinkStyle =
    let
        shrinkingChildlen =
            String.join ","
                [ "." ++ flexRowClass ++ ">." ++ flexShrinkClass
                , "." ++ flexColumnClass ++ ">." ++ flexShrinkClass
                ]
    in
    s shrinkingChildlen [ ( "flex-shrink", "1" ) ]


flexShrinkClass : String
flexShrinkClass =
    "fs"


flexCenterStyle : Style
flexCenterStyle =
    let
        shrinkingChildlen =
            String.join ","
                [ "." ++ flexRowClass ++ "." ++ flexCenterClass
                , "." ++ flexColumnClass ++ "." ++ flexCenterClass
                ]
    in
    s shrinkingChildlen [ ( "align-items", "center" ) ]


flexCenterClass : String
flexCenterClass =
    "fcenter"


paddingStyle : Int -> Style
paddingStyle pad =
    c (paddingClass pad) [ ( "padding", px pad ) ]


paddingClass : Int -> String
paddingClass pad =
    "pa" ++ String.fromInt pad


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
    ".fi" :: autoFlexItemTags


spacingRowClass : Int -> String
spacingRowClass space =
    "sr" ++ String.fromInt space


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
    "sc" ++ String.fromInt space
