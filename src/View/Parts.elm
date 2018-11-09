module View.Parts exposing
    ( noneAttr, visible, switchCursor, inputScreen, dragHandle
    , breakP, breakT, breakTColumn, collapsingColumn
    , octiconEl, squareIconOrHeadEl, iconWithBadgeEl
    , textInputEl, squareButtonEl, roundButtonEl, rectButtonEl, thinButtonEl
    , primaryButtonEl, successButtonEl, dangerButtonEl
    , scale12, css, brightness, setAlpha, manualStyle
    , filtersToIconEl
    , discordGuildIconEl, discordChannelEl
    , fixedColumnWidth, rectElementRound, spacingUnit, rectElementOuterPadding, rectElementInnerPadding
    , columnAreaParentId, defaultOcticonColor
    )

{-| View parts, complementing Element and Html.


## Essenstials

@docs noneAttr, visible, switchCursor, inputScreen, dragHandle
@docs breakP, breakT, breakTColumn, collapsingColumn


## Icons

@docs octiconEl, squareIconOrHeadEl, iconWithBadgeEl


## Inputs

@docs textInputEl, squareButtonEl, roundButtonEl, rectButtonEl, thinButtonEl
@docs primaryButtonEl, successButtonEl, dangerButtonEl


## Styles

@docs scale12, css, brightness, setAlpha, manualStyle


## Column Filter

@docs filtersToIconEl


## Discord

@docs discordGuildIconEl, discordChannelEl


## Constants

@docs fixedColumnWidth, rectElementRound, spacingUnit, rectElementOuterPadding, rectElementInnerPadding
@docs columnAreaParentId, defaultOcticonColor

-}

import Array exposing (Array)
import Data.ColorTheme exposing (ColorTheme, oneDark)
import Data.Filter exposing (Filter, FilterAtom(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial)
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Lazy exposing (..)
import Extra exposing (ite)
import Html
import Html.Attributes exposing (class, draggable, style)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode
import ListExtra
import Octicons


noneAttr : Attribute msg
noneAttr =
    htmlAttribute (Html.Attributes.property "none" Json.Encode.null)


{-| Hide an Element with `display: none;` property.

When supported by popular browsers, it might be better replaced by `visibility: collapse`.

This is actually crucial for rendering performance.
Since entirely removing DOMs with `none` and re-introducing them afterwards
causes significant computational overhead for web browsers.

As a rule of thumb, if the DOM tree can be reused, do not delete it, hide it instead.
If it is really gone, delete it.

-}
visible : Bool -> Attribute msg
visible isVisible =
    if isVisible then
        noneAttr

    else
        htmlAttribute (style "display" "none")


{-| Cursor helper for input elements. When False, use cursor: default;
-}
switchCursor : Bool -> Attribute msg
switchCursor enabled =
    ite enabled noneAttr (htmlAttribute (style "cursor" "default"))


octiconEl : List (Attribute msg) -> { size : Int, color : Color, shape : Octicons.Options -> Html.Html msg } -> Element msg
octiconEl attrs { size, color, shape } =
    Octicons.defaultOptions
        |> Octicons.color (css color)
        |> Octicons.size size
        |> shape
        |> html
        |> el attrs


squareIconOrHeadEl : List (Attribute msg) -> { size : Int, name : String, url : Maybe String } -> Element msg
squareIconOrHeadEl userAttrs { size, name, url } =
    let
        ( attr, fallbackContent ) =
            case url of
                Just url_ ->
                    ( BG.uncropped url_, none )

                Nothing ->
                    ( Font.size (size // 2), el [ centerX, centerY ] (text (String.left 1 name)) )

        attrs =
            [ width (px size)
            , height (px size)
            , alignTop
            , BG.color iconBackground
            , BD.rounded (iconRounding size)
            , clip
            , htmlAttribute (Html.Attributes.title name)
            , attr
            ]
                ++ userAttrs
    in
    el attrs fallbackContent


iconBackground : Color
iconBackground =
    oneDark.sub


iconRounding : Int -> Int
iconRounding badgeSize =
    max 2 (badgeSize // 10)


iconWithBadgeEl :
    List (Attribute msg)
    ->
        { size : Int
        , badge : Maybe (Int -> Element msg)
        , fallback : String
        , url : Maybe String
        }
    -> Element msg
iconWithBadgeEl userAttrs { size, badge, fallback, url } =
    let
        bottomRightBadgeAttrs =
            case badge of
                Just badgeEl ->
                    let
                        badgeSize =
                            size // 3
                    in
                    [ alignTop
                    , inFront <|
                        el
                            [ alignBottom
                            , alignRight
                            , BD.rounded (iconRounding badgeSize)
                            , clip
                            ]
                            (badgeEl badgeSize)
                    ]

                Nothing ->
                    [ alignTop ]

        innerIconPadding =
            max 1 (size // 20)
    in
    el (bottomRightBadgeAttrs ++ userAttrs) <|
        squareIconOrHeadEl [ padding innerIconPadding ]
            { size = size - (innerIconPadding * 2), name = fallback, url = url }


textInputEl :
    { onChange : String -> msg
    , theme : ColorTheme
    , enabled : Bool
    , text : String
    , label : Element.Input.Label msg
    , placeholder : Maybe (Element msg)
    }
    -> Element msg
textInputEl { onChange, theme, enabled, text, label, placeholder } =
    Element.Input.text
        [ width fill
        , height fill
        , padding textInputPadding
        , BG.color theme.note
        , BD.width 0
        , BD.rounded rectElementRound
        , Font.color theme.text
        , switchCursor enabled
        , customPlaceholder theme placeholder text
        , inputScreen enabled
        , htmlAttribute (style "line-height" "1") -- Cancelling line-height introduced by elm-ui
        , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
        ]
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , label = label
        }


textInputPadding : Int
textInputPadding =
    5


customPlaceholder : ColorTheme -> Maybe (Element msg) -> String -> Attribute msg
customPlaceholder theme phMaybe text =
    -- elm-ui's placeholder uses opacity to switch visibility, but it triggers style recalculation on change.
    -- Whereas display property does not trigger style recalculation, and with inFront (position: absolute;), no layout/reflow.
    case phMaybe of
        Just ph ->
            inFront <|
                el
                    [ padding textInputPadding
                    , centerY
                    , visible (String.isEmpty text)
                    , Font.color (setAlpha 0.5 theme.text)
                    ]
                    ph

        Nothing ->
            noneAttr


primaryButtonEl :
    List (Attribute msg)
    ->
        { onPress : msg
        , width : Length
        , theme : ColorTheme
        , enabled : Bool
        , innerElement : Element msg
        }
    -> Element msg
primaryButtonEl attrs { onPress, width, theme, enabled, innerElement } =
    rectButtonEl attrs
        { onPress = onPress
        , width = width
        , enabledColor = theme.prim
        , enabledFontColor = theme.text
        , enabled = enabled
        , innerElement = innerElement
        }


successButtonEl :
    List (Attribute msg)
    ->
        { onPress : msg
        , width : Length
        , theme : ColorTheme
        , enabled : Bool
        , innerElement : Element msg
        }
    -> Element msg
successButtonEl attrs { onPress, width, theme, enabled, innerElement } =
    rectButtonEl attrs
        { onPress = onPress
        , width = width
        , enabledColor = theme.succ
        , enabledFontColor = theme.text
        , enabled = enabled
        , innerElement = innerElement
        }


dangerButtonEl :
    List (Attribute msg)
    ->
        { onPress : msg
        , width : Length
        , theme : ColorTheme
        , enabled : Bool
        , innerElement : Element msg
        }
    -> Element msg
dangerButtonEl attrs { onPress, width, theme, enabled, innerElement } =
    rectButtonEl attrs
        { onPress = onPress
        , width = width
        , enabledColor = theme.err
        , enabledFontColor = theme.text
        , enabled = enabled
        , innerElement = innerElement
        }


rectButtonEl :
    List (Attribute msg)
    ->
        { onPress : msg
        , width : Length
        , enabledColor : Color
        , enabledFontColor : Color
        , enabled : Bool
        , innerElement : Element msg
        }
    -> Element msg
rectButtonEl userAttrs { onPress, width, enabledColor, enabledFontColor, enabled, innerElement } =
    let
        attrs =
            [ Element.width width
            , padding rectButtonPadding
            , BD.rounded rectElementRound
            , BG.color enabledColor
            , Font.color enabledFontColor
            , clip
            , inputScreen enabled
            , switchCursor enabled
            ]
                ++ userAttrs
    in
    Element.Input.button attrs
        { onPress = ite enabled (Just onPress) Nothing
        , label = el [ centerX, centerY ] innerElement
        }


rectButtonPadding : Int
rectButtonPadding =
    10


{-| Concealing screen over input elements, hiding it when disabled (False).

This is introduced in order to reduce style recalculation.
In elm-ui, changing color of an element causes replacing of class and style element,
thus triggering style recalc, which can be costly if there are thousands of DOM elements to traverse.

This, on the other hand, places static element with transparent black background,
over the target input element. And when the input is enabled, its turned off via display: none;
whcih does not cause style recalc nor layout/reflow (due to inFront == position: absolute;)

-}
inputScreen : Bool -> Attribute msg
inputScreen enabled =
    inFront <| el [ width fill, height fill, visible (not enabled), BG.color (rgba255 0 0 0 0.5) ] none


thinButtonEl :
    List (Attribute msg)
    ->
        { onPress : msg
        , width : Length
        , enabledColor : Color
        , enabledFontColor : Color
        , enabled : Bool
        , innerElement : Element msg
        }
    -> Element msg
thinButtonEl userAttrs { onPress, width, enabledColor, enabledFontColor, enabled, innerElement } =
    let
        attrs =
            [ Element.width width
            , padding thinButtonPadding
            , BD.rounded thinButtonPadding
            , BG.color enabledColor
            , Font.color enabledFontColor
            , clip
            , inputScreen enabled
            , switchCursor enabled
            ]
                ++ userAttrs
    in
    Element.Input.button attrs
        { onPress = ite enabled (Just onPress) Nothing
        , label = el [ centerX, centerY ] innerElement
        }


thinButtonPadding : Int
thinButtonPadding =
    2


{-| Unlike general-purpose rectButtonEl, this cannot control BG/Font.color on enabled/disabled,
since it expects avatar images/octicons in its element.
Their color or saturations must be controlled by callers.
-}
roundButtonEl :
    { onPress : msg
    , enabled : Bool
    , innerElement : Element msg
    , innerElementSize : Int
    }
    -> Element msg
roundButtonEl { onPress, enabled, innerElement, innerElementSize } =
    Element.Input.button
        [ width (px innerElementSize)
        , height (px innerElementSize)
        , BD.rounded (innerElementSize // 2 + 1)
        , switchCursor enabled
        , roundInputScreen innerElementSize enabled
        , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
        ]
        { onPress = ite enabled (Just onPress) Nothing
        , label = el [ centerX, centerY ] innerElement
        }


roundInputScreen : Int -> Bool -> Attribute msg
roundInputScreen size enabled =
    inFront <|
        el
            [ width fill
            , height fill
            , BD.rounded (size // 2 + 1)
            , visible (not enabled)
            , BG.color (rgba255 0 0 0 0.5)
            ]
            none


squareButtonEl :
    List (Attribute msg)
    ->
        { onPress : msg
        , enabled : Bool
        , innerElement : Element msg
        , innerElementSize : Int
        }
    -> Element msg
squareButtonEl userAttrs { onPress, enabled, innerElement, innerElementSize } =
    let
        attrs =
            [ width (px innerElementSize)
            , height (px innerElementSize)
            , clip
            , switchCursor enabled
            , inputScreen enabled
            , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
            ]
                ++ userAttrs
    in
    Element.Input.button attrs { onPress = ite enabled (Just onPress) Nothing, label = el [ centerX, centerY ] innerElement }


{-| Text that can break on parent inline element width.
Respects "word-break" and "white-space" styles.

This is a workaround for <https://github.com/mdgriffith/elm-ui/issues/49>

-}
breakT : String -> Element msg
breakT =
    Html.text >> html


{-| `paragraph` with "word-break: break-all" and "white-space: pre-wrap".

Suitable for user-generated texts. Use with `breakT`.

-}
breakP : List (Attribute msg) -> List (Element msg) -> Element msg
breakP attrs =
    paragraph <| attrs ++ [ htmlAttribute (class breakClassName) ]


breakClassName : String
breakClassName =
    "breakEl"


{-| `textColumn` with "word-break: break-all" and "white-space: pre-wrap".
-}
breakTColumn : List (Attribute msg) -> List (Element msg) -> Element msg
breakTColumn attrs =
    textColumn <| htmlAttribute (class breakClassName) :: attrs


collapsingColumn : List (Attribute msg) -> List (Element msg) -> Element msg
collapsingColumn attrs elements =
    case elements of
        [] ->
            none

        _ ->
            column attrs elements



-- STYLE HELPER


{-| Scale 12px by a power of 1.25. Primarily meant for font sizes.
-}
scale12 : Int -> Int
scale12 =
    modular 12 1.25 >> round


{-| Shift brightness of a Color (RGB) by a power of 1.15, without altering alpha.

`brightness 1` on a Color of `{red = 100, green = 100, blue = 100}`
will yield `{red = 115, green = 115, blue = 115}`.

`brightness -1` on the same Color will yield `{red = 86.96, green = 86.96, blue = 86.96}`

-}
brightness : Float -> Color -> Color
brightness power color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgb (red * (1.15 ^ power)) (green * (1.15 ^ power)) (blue * (1.15 ^ power))


setAlpha : Float -> Color -> Color
setAlpha a color =
    let
        rgba =
            toRgb color
    in
    fromRgb { rgba | alpha = a }


{-| Dump a Color to CSS-compatible representaiton
-}
css : Color -> String
css color =
    let
        { red, green, blue } =
            toRgb color
    in
    String.join ""
        [ "rgb("
        , String.fromFloat (255 * red)
        , ","
        , String.fromFloat (255 * green)
        , ","
        , String.fromFloat (255 * blue)
        , ")"
        ]


dragHandle : Decoder msg -> List (Attribute msg)
dragHandle onDragstart =
    [ htmlAttribute (draggable "true")
    , htmlAttribute (class dragHandleClassName)
    , htmlAttribute (Html.Events.stopPropagationOn "dragstart" (D.map (\msg -> ( msg, True )) onDragstart))
    ]


dragHandleClassName : String
dragHandleClassName =
    "dragHandle"


filtersToIconEl : List (Attribute msg) -> { size : Int, fam : FilterAtomMaterial, filters : Array Filter } -> Element msg
filtersToIconEl attrs { size, fam, filters } =
    filters
        |> Array.foldl (filterToIconEl attrs size fam) Nothing
        |> Maybe.withDefault (lazy2 fallbackIconEl attrs size)


filterToIconEl : List (Attribute msg) -> Int -> FilterAtomMaterial -> Filter -> Maybe (Element msg) -> Maybe (Element msg)
filterToIconEl attrs size fam filter elMaybe =
    let
        reducer filterAtom acc =
            case ( acc, filterAtom ) of
                ( Just _, _ ) ->
                    acc

                ( _, OfDiscordChannel cId ) ->
                    FAM.mapDiscordChannel cId fam (discordChannelIconEl attrs size)

                ( _, _ ) ->
                    Nothing
    in
    Data.Filter.fold reducer elMaybe filter


discordChannelIconEl : List (Attribute msg) -> Int -> Discord.ChannelCache -> Element msg
discordChannelIconEl attrs size c =
    case c.guildMaybe of
        Just guild ->
            iconWithBadgeEl attrs
                { size = size
                , badge = Just (lazy discordBadgeEl)
                , fallback = c.name
                , url = Maybe.map (Discord.imageUrlNoFallback (Just size)) guild.icon
                }

        Nothing ->
            iconWithBadgeEl attrs
                { size = size
                , badge = Nothing
                , fallback = c.name
                , url = Just (Discord.defaultIconUrl (Just size))
                }


discordBadgeEl : Int -> Element msg
discordBadgeEl badgeSize =
    el
        [ width (px badgeSize)
        , height (px badgeSize)
        , BG.uncropped (Discord.defaultIconUrl (Just badgeSize))
        ]
        none


fallbackIconEl : List (Attribute msg) -> Int -> Element msg
fallbackIconEl userAttrs size =
    let
        attrs =
            [ width (px size)
            , height (px size)
            , clip
            , BD.width 1
            , BD.color oneDark.note
            , BD.rounded rectElementRound
            , Font.size (size // 2)
            , Font.family [ Font.serif ]
            ]
                ++ userAttrs
    in
    el attrs <| el [ centerX, centerY ] <| text "Z"


discordGuildIconEl : List (Attribute msg) -> Int -> Discord.Guild -> Element msg
discordGuildIconEl attrs size guild =
    squareIconOrHeadEl attrs
        { size = size
        , name = guild.name
        , url = Maybe.map (Discord.imageUrlNoFallback (Just size)) guild.icon
        }


discordChannelEl : List (Attribute msg) -> { size : Int, channel : { x | name : String, guildMaybe : Maybe Discord.Guild } } -> Element msg
discordChannelEl attrs { size, channel } =
    row ([ spacing discordGuildIconSpacingX ] ++ attrs)
        [ channel.guildMaybe |> Maybe.map (discordGuildIconEl [] size) |> Maybe.withDefault none
        , text ("#" ++ channel.name)
        ]


discordGuildIconSpacingX : Int
discordGuildIconSpacingX =
    2



-- MANUAL STYLE


manualStyle : Html.Html msg
manualStyle =
    Html.node "style"
        []
        [ Html.text "*{scroll-behavior:smooth;}" -- Smooth-scrolling on JS-invoked scrolls
        , Html.text "::-webkit-scrollbar{display:none;}" -- Hidden scrollbars
        , Html.text <| "." ++ breakClassName ++ "{white-space:pre-wrap!important;word-break:break-all!important;}" -- Breakable inline texts
        , Html.text <| "." ++ dragHandleClassName ++ "{cursor:all-scroll;}" -- Drag handle cursor
        , Html.text ":focus{box-shadow:0px 0px 3px 3px rgb(103,123,196);outline:none;}" -- Manual focus style
        , Html.text "a:link{text-decoration:none;}" -- Disabled browser-default link-underlining
        , Html.text "a:link:hover{text-decoration:underline;}" -- Workaround for underline not being appliable to mouseOver or focused
        ]



-- CONSTANTS


fixedColumnWidth : Int
fixedColumnWidth =
    350


spacingUnit : Int
spacingUnit =
    5


rectElementRound : Int
rectElementRound =
    spacingUnit


rectElementOuterPadding : Int
rectElementOuterPadding =
    spacingUnit * 2


rectElementInnerPadding : Int
rectElementInnerPadding =
    spacingUnit


columnAreaParentId : String
columnAreaParentId =
    "columnAreaParent"


defaultOcticonColor : Color
defaultOcticonColor =
    oneDark.note
