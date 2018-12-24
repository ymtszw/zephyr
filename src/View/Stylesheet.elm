module View.Stylesheet exposing (render)

import Html
import View.Atom.Typography as Typography
import View.Style exposing (Style, px, s, scale12)


{-| Dumps all required stylesheet in the app.
-}
render : Html.Html msg
render =
    Html.node "style" [] <|
        List.map (View.Style.toString >> Html.text) <|
            (preamble ++ Typography.styles)


preamble : List Style
preamble =
    resetUserAgentStyles ++ globalStyles


{-| <http://meyerweb.com/eric/tools/css/reset/>
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
        , ( "font-size", px (scale12 0) ) -- Global default font-size is 12px
        , ( "font", "inherit" )
        , ( "vertical-align", "baseline" )
        ]
    , s "article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section"
        [ ( "display", "block" ) ]
    , s "body"
        [ ( "line-height", "1" )
        , ( "font-size", px (scale12 0) )
        ]
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
        [ ( "box-shadow", "0px 0px 3px 3px rgb(103,123,196)" )
        , ( "outline", "none" )
        ]
    , -- When we use images as a cosmetic tokens, we almost always need them to be rendered as blocks.
      -- Override when inlining is explicitly needed
      s "img" [ ( "display", "block" ) ]
    , s "a:link" [ ( "text-decoration", "none" ) ]
    , s "a:link:hover" [ ( "text-decoration", "underline" ) ]
    ]
