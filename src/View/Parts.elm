module View.Parts exposing
    ( noneAttr, breakP, breakT, breakTColumn, collapsingColumn, dragHandle
    , octiconEl, squareIconOrHeadEl, iconWithBadgeEl
    , textInputEl, squareButtonEl, roundButtonEl, rectButtonEl, primaryButtonEl, dangerButtonEl
    , scale12, css, brightness, setAlpha, manualStyle
    , filtersToIconEl
    , discordGuildIconEl
    , fixedColumnWidth, rectElementRound, spacingUnit, rectElementOuterPadding, rectElementInnerPadding
    , columnAreaParentId, defaultOcticonColor
    )

{-| View parts, complementing Element and Html.


## Essenstials

@docs noneAttr, breakP, breakT, breakTColumn, collapsingColumn, dragHandle


## Icons

@docs octiconEl, squareIconOrHeadEl, iconWithBadgeEl


## Inputs

@docs textInputEl, squareButtonEl, roundButtonEl, rectButtonEl, primaryButtonEl, dangerButtonEl


## Styles

@docs scale12, css, brightness, setAlpha, manualStyle


## Filter

@docs filtersToIconEl


## Discord

@docs discordGuildIconEl


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


octiconEl : { size : Int, color : Color, shape : Octicons.Options -> Html.Html msg } -> Element msg
octiconEl { size, color, shape } =
    Octicons.defaultOptions
        |> Octicons.color (css color)
        |> Octicons.size size
        |> shape
        |> html


squareIconOrHeadEl : Int -> String -> Maybe String -> Element msg
squareIconOrHeadEl size name urlMaybe =
    let
        ( attr, fallbackContent ) =
            case urlMaybe of
                Just url ->
                    ( BG.uncropped url, none )

                Nothing ->
                    ( Font.size (size // 2), el [ centerX, centerY ] (text (String.left 1 name)) )
    in
    el
        [ width (px size)
        , height (px size)
        , alignTop
        , BG.color iconBackground
        , BD.rounded (iconRounding size)
        , clip
        , htmlAttribute (Html.Attributes.title name)
        , attr
        ]
        fallbackContent


iconBackground : Color
iconBackground =
    oneDark.sub


iconRounding : Int -> Int
iconRounding badgeSize =
    max 2 (badgeSize // 10)


iconWithBadgeEl :
    { size : Int
    , badge : Maybe (Int -> Element msg)
    , fallback : String
    , url : Maybe String
    }
    -> Element msg
iconWithBadgeEl { size, badge, fallback, url } =
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
    squareIconOrHeadEl (size - (innerIconPadding * 2)) fallback url
        |> el [ padding innerIconPadding ]
        |> el bottomRightBadgeAttrs


textInputEl :
    { onChange : String -> msg
    , theme : ColorTheme
    , enabled : Bool
    , text : String
    , label : Element.Input.Label msg
    , placeholder : Maybe (Element.Input.Placeholder msg)
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
        , htmlAttribute (style "line-height" "1") -- Cancelling line-height introduced by elm-ui
        , ite enabled noneAttr (htmlAttribute (style "cursor" "default"))
        , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
        ]
        { onChange = onChange
        , text = text
        , placeholder = placeholder
        , label = label
        }


textInputPadding : Int
textInputPadding =
    5


primaryButtonEl :
    { onPress : msg
    , width : Length
    , theme : ColorTheme
    , enabled : Bool
    , innerElement : Element msg
    }
    -> Element msg
primaryButtonEl { onPress, width, theme, enabled, innerElement } =
    rectButtonEl
        { onPress = onPress
        , width = width
        , enabledColor = theme.prim
        , enabledFontColor = theme.text
        , disabledColor = theme.sub
        , disabledFontColor = theme.note
        , enabled = enabled
        , innerElement = innerElement
        }


dangerButtonEl :
    { onPress : msg
    , width : Length
    , theme : ColorTheme
    , enabled : Bool
    , innerElement : Element msg
    }
    -> Element msg
dangerButtonEl { onPress, width, theme, enabled, innerElement } =
    rectButtonEl
        { onPress = onPress
        , width = width
        , enabledColor = theme.err
        , enabledFontColor = theme.text
        , disabledColor = theme.sub
        , disabledFontColor = theme.note
        , enabled = enabled
        , innerElement = innerElement
        }


rectButtonEl :
    { onPress : msg
    , width : Length
    , enabledColor : Color
    , enabledFontColor : Color
    , disabledColor : Color
    , disabledFontColor : Color
    , enabled : Bool
    , innerElement : Element msg
    }
    -> Element msg
rectButtonEl { onPress, width, enabledColor, enabledFontColor, disabledColor, disabledFontColor, enabled, innerElement } =
    Element.Input.button
        [ Element.width width
        , padding rectButtonPadding
        , BD.rounded rectElementRound
        , BG.color (ite enabled enabledColor disabledColor)
        , Font.color (ite enabled enabledFontColor disabledFontColor)
        , clip
        , ite enabled noneAttr (htmlAttribute (style "cursor" "default"))
        , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
        ]
        { onPress = ite enabled (Just onPress) Nothing
        , label = el [ centerX, centerY ] innerElement
        }


rectButtonPadding : Int
rectButtonPadding =
    10


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
        , ite enabled noneAttr (htmlAttribute (style "cursor" "default"))
        , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
        ]
        { onPress = ite enabled (Just onPress) Nothing
        , label = el [ centerX, centerY ] innerElement
        }


squareButtonEl :
    { onPress : msg
    , enabled : Bool
    , innerElement : Element msg
    , innerElementSize : Int
    }
    -> Element msg
squareButtonEl { onPress, enabled, innerElement, innerElementSize } =
    Element.Input.button
        [ width (px innerElementSize)
        , height (px innerElementSize)
        , clip
        , ite enabled noneAttr (htmlAttribute (style "cursor" "default"))
        , ite enabled noneAttr (htmlAttribute (Html.Attributes.disabled True))
        ]
        { onPress = ite enabled (Just onPress) Nothing
        , label = el [ centerX, centerY ] innerElement
        }


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


filtersToIconEl : Int -> FilterAtomMaterial -> Array Filter -> Element msg
filtersToIconEl size fam filters =
    filters
        |> Array.foldl (filterToIconEl size fam) Nothing
        |> Maybe.withDefault (lazy fallbackIconEl size)


filterToIconEl : Int -> FilterAtomMaterial -> Filter -> Maybe (Element msg) -> Maybe (Element msg)
filterToIconEl size fam filter elMaybe =
    let
        reducer filterAtom acc =
            case ( acc, filterAtom ) of
                ( Just _, _ ) ->
                    acc

                ( _, OfDiscordChannel cId ) ->
                    FAM.mapDiscordChannel cId fam (discordChannelIconEl size)

                ( _, _ ) ->
                    Nothing
    in
    Data.Filter.fold reducer elMaybe filter


discordChannelIconEl : Int -> Discord.ChannelCache -> Element msg
discordChannelIconEl size c =
    case c.guildMaybe of
        Just guild ->
            iconWithBadgeEl
                { size = size
                , badge = Just (lazy discordBadgeEl)
                , fallback = c.name
                , url = Maybe.map (Discord.imageUrlNoFallback (Just size)) guild.icon
                }

        Nothing ->
            iconWithBadgeEl
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


fallbackIconEl : Int -> Element msg
fallbackIconEl size =
    el
        [ width (px size)
        , height (px size)
        , clip
        , BD.width 1
        , BD.color oneDark.note
        , BD.rounded rectElementRound
        , Font.size (size // 2)
        , Font.family [ Font.serif ]
        ]
        (el [ centerX, centerY ] <| text "Z")


discordGuildIconEl : Int -> Discord.Guild -> Element msg
discordGuildIconEl size guild =
    guild.icon
        |> Maybe.map (Discord.imageUrlNoFallback (Just size))
        |> squareIconOrHeadEl size guild.name



-- MANUAL STYLE


manualStyle : Html.Html msg
manualStyle =
    Html.node "style"
        []
        [ Html.text "*{scroll-behavior:smooth;}"
        , Html.text "::-webkit-scrollbar{display:none;}"
        , Html.text <| "." ++ breakClassName ++ "{white-space:pre-wrap!important;word-break:break-all!important;}"
        , Html.text <| "." ++ dragHandleClassName ++ "{cursor:all-scroll;}"
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
