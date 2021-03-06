module View.Stylesheet exposing (length, render)

import Html
import View.Atoms.Animation as Animation
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Button as Button
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Input as Input
import View.Atoms.Input.Select as Select
import View.Atoms.Layout as Layout
import View.Atoms.TextBlock as TextBlock
import View.Atoms.Typography as Typography
import View.Molecules.Icon as Icon
import View.Molecules.MediaViewer as MediaViewer
import View.Molecules.ProducerConfig as ProducerConfig
import View.Molecules.RawColumnItem as RawColumnItem
import View.Molecules.Source as Source
import View.Molecules.Table as Table
import View.Molecules.Wallpaper as Wallpaper
import View.Organisms.Column.Header as Header
import View.Organisms.Column.Items as Items
import View.Organisms.Column.NewMessageEditor as NewMessageEditor
import View.Organisms.Config.Pref as Pref
import View.Organisms.Modeless as Modeless
import View.Organisms.Sidebar as Sidebar
import View.Style exposing (..)
import View.Templates.Main


{-| Dumps all required stylesheet of the app as a single `<style>` node.
-}
render : Html.Html msg
render =
    Html.node "style" [] [ Html.text rendered ]


rendered : String
rendered =
    String.join "" <|
        List.map View.Style.toString <|
            -- XXX Order matters!!
            preamble
                ++ Typography.styles
                ++ TextBlock.styles
                ++ Button.styles
                ++ Select.styles
                ++ Input.styles
                ++ Border.styles
                ++ Background.styles
                ++ Layout.styles
                ++ Image.styles
                ++ Animation.styles
                ++ Cursor.styles
                ++ Icon.styles
                ++ Wallpaper.styles
                ++ Table.styles
                ++ Source.styles
                ++ ProducerConfig.styles
                ++ RawColumnItem.styles
                ++ MediaViewer.styles
                ++ Sidebar.styles
                ++ Modeless.styles
                ++ Pref.styles
                ++ Header.styles
                ++ NewMessageEditor.styles
                ++ Items.styles
                ++ View.Templates.Main.styles


length : Int
length =
    String.length rendered


preamble : List Style
preamble =
    resetUserAgentStyles ++ globalStyles


{-| Resets CSS, and apply some global defaults.

Based on:

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
                , "input,textarea,button" -- Added
                ]
    in
    [ s basicTags
        [ ( "margin", "0" )
        , ( "padding", "0" )
        , ( "border", "0" )
        , ( "font-size", "inherit" ) -- These two inheritances are necessary for cascading ancestors' font settings to descendants
        , ( "font", "inherit" )
        , ( "vertical-align", "baseline" )
        ]
    , s "article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section"
        [ ( "display", "block" ) ]
    , s "body" [ ( "line-height", "1" ) ]
        |> inject Typography.regularStyle
        |> inject Typography.sansSerifStyle
        |> inject Background.oneDarkMainStyle
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
        [ ( "box-sizing", "border-box" ) -- Really, WHO needs other than border-box??? JK
        ]
    , s ":focus"
        [ ( "box-shadow", "0px 0px 3px 1px #677bc4" )
        , ( "outline", "none" )
        ]
    , s "input,textarea,button" [ ( "color", "inherit" ) ]
    , s "hr" [ ( "width", "100%" ), ( "border", "none" ) ] -- Erase user-agent's hr style, apply in Border module
    ]
