module View.Stylesheet exposing (render)

import Html
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout as Layout
import View.Atom.TextBlock as TextBlock
import View.Atom.Typography as Typography
import View.Style exposing (Style, inject, px, s, scale12)


{-| Dumps all required stylesheet of the app as a single `<style>` node.
-}
render : Html.Html msg
render =
    Html.node "style" [] <|
        List.map (View.Style.toString >> Html.text) <|
            preamble
                ++ Typography.styles
                ++ TextBlock.styles
                ++ Border.styles
                ++ Background.styles
                ++ Layout.styles


preamble : List Style
preamble =
    resetUserAgentStyles ++ globalStyles


{-| Resets CSS, and apply some global defaults.

<http://meyerweb.com/eric/tools/css/reset/>
v2.0 | 20110126
License: none (public domain)

-}
resetUserAgentStyles : List Style
resetUserAgentStyles =
    let
        basicTags =
            String.join ","
                [ "html,body,div,span,applet,object,iframe"
                , "h1,h2,h3,h4,h5,h6,p,blockquote,pre"
                , "a,abbr,acronym,address,big,cite,code"
                , "del,dfn,em,img,ins,kbd,q,s,samp"
                , "small,strike,strong,sub,sup,tt,var"
                , "b,u,i,center"
                , "dl,dt,dd,ol,ul,li"
                , "fieldset,form,label,legend"
                , "table,caption,tbody,tfoot,thead,tr,th,td"
                , "article,aside,canvas,details,embed"
                , "figure,figcaption,footer,header,hgroup"
                , "menu,nav,output,ruby,section,summary"
                , "time,mark,audio,video"
                ]
    in
    [ s basicTags
        [ ( "margin", "0" )
        , ( "padding", "0" )
        , ( "border", "0" )
        , ( "font-size", "inherit" ) -- These two inheritances are necessary for cascading parents' font settings to children
        , ( "font", "inherit" )
        , ( "vertical-align", "baseline" )
        ]
    , s "article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section"
        [ ( "display", "block" ) ]
    , s "body" [ ( "line-height", "1" ) ]
        |> inject Typography.sizeBaseStyle
        |> inject Typography.sansSerifStyle
        |> inject Background.oneDarkDefaultStyle
    , s "ol,ul"
        [ ( "list-style", "none" ) ]
    , s "blockquote,q"
        [ ( "quotes", "none" ) ]
    , s "blockquote:before,blockquote:after,q:before,q:after"
        [ ( "content", "''" ), ( "content", "none" ) ]
    , s "table"
        [ ( "border-collapse", "collapse" ), ( "border-spacing", "0" ) ]
    ]


globalStyles : List Style
globalStyles =
    [ -- Hidden scrollbars, Webkit only
      s "::-webkit-scrollbar" [ ( "display", "none" ) ]
    , s "*"
        [ ( "scroll-behavior", "smooth" )
        , ( "box-sizing", "border-box" ) -- Really, WHO needs other than border-box??? JK
        ]
    , s ":focus"
        [ ( "box-shadow", "0px 0px 3px 3px #677bc4" )
        , ( "outline", "none" )
        ]
    , -- When we use images as a cosmetic tokens, we almost always need them to be rendered as blocks.
      -- Override when inlining is explicitly needed
      s "img" [ ( "display", "block" ) ]
    ]
