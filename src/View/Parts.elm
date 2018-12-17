module View.Parts exposing
    ( noneAttr, style, visible, switchCursor, borderFlash, rotating, wiggle, onAnimationEnd
    , breakP, breakT, breakTColumn, collapsingColumn, inputScreen, dragHandle
    , scale12, cssRgba, brightness, setAlpha, manualStyle
    , octiconEl, squareIconOrHeadEl, iconWithBadgeEl
    , textInputEl, multilineInputEl, toggleInputEl, squareButtonEl, roundButtonEl, rectButtonEl, thinButtonEl
    , primaryButtonEl, successButtonEl, dangerButtonEl
    , filtersToIconEl, filtersToTextEl, fetchStatusTextEl
    , discordGuildIconEl, discordChannelEl, slackTeamIconEl, slackLogoClippedEl, slackConversationEl
    , columnWidth, columnHeaderHeight, columnHeaderIconSize, columnPinColor, columnBorderWidth, columnAreaParentId
    , columnItemMinimumHeight, columnItemBorderBottom, columnItemAvatarSize
    , spacingUnit, rectElementRound, rectElementOuterPadding, rectElementInnerPadding
    , defaultOcticonColor, trashcanPaddingAdjust
    )

{-| View parts, complementing Element and Html.


## Helpers

@docs noneAttr, style, visible, switchCursor, borderFlash, rotating, wiggle, onAnimationEnd
@docs breakP, breakT, breakTColumn, collapsingColumn, inputScreen, dragHandle
@docs scale12, cssRgba, brightness, setAlpha, manualStyle


## Elements

@docs octiconEl, squareIconOrHeadEl, iconWithBadgeEl
@docs textInputEl, multilineInputEl, toggleInputEl, squareButtonEl, roundButtonEl, rectButtonEl, thinButtonEl
@docs primaryButtonEl, successButtonEl, dangerButtonEl
@docs filtersToIconEl, filtersToTextEl, fetchStatusTextEl
@docs discordGuildIconEl, discordChannelEl, slackTeamIconEl, slackLogoClippedEl, slackConversationEl


## Constants

@docs columnWidth, columnHeaderHeight, columnHeaderIconSize, columnPinColor, columnBorderWidth, columnAreaParentId
@docs columnItemMinimumHeight, columnItemBorderBottom, columnItemAvatarSize
@docs spacingUnit, rectElementRound, rectElementOuterPadding, rectElementInnerPadding
@docs defaultOcticonColor, trashcanPaddingAdjust

-}

import Array exposing (Array)
import Data.ColorTheme exposing (ColorTheme, aubergine, oneDark)
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial)
import Data.Producer.Discord as Discord
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus)
import Data.Producer.Slack as Slack
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (..)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode
import ListExtra
import Octicons
import Time
import TimeExtra
import Url


noneAttr : Attribute msg
noneAttr =
    htmlAttribute (Html.Attributes.property "none" Json.Encode.null)


style : String -> String -> Attribute msg
style prop value =
    htmlAttribute (Html.Attributes.style prop value)


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
        style "display" "none"


{-| Cursor helper for input elements. When False, use cursor: default;
-}
switchCursor : Bool -> Attribute msg
switchCursor enabled =
    if enabled then
        noneAttr

    else
        style "cursor" "default"


borderFlash : Bool -> Attribute msg
borderFlash doFlash =
    if doFlash then
        style "animation" <| "2s ease-out " ++ borderFlashKeyframesName

    else
        noneAttr


borderFlashKeyframesName : String
borderFlashKeyframesName =
    "borderFlash"


rotating : Bool -> Attribute msg
rotating doRotate =
    if doRotate then
        style "animation" <| "1.5s linear 0s infinite " ++ rotatingKeyframesName

    else
        noneAttr


rotatingKeyframesName : String
rotatingKeyframesName =
    "rotating"


wiggle : Attribute msg
wiggle =
    style "animation" <| "1s ease-in-out 0s 3 " ++ wiggleKeyframesName


wiggleKeyframesName : String
wiggleKeyframesName =
    "wiggle"


{-| Fired when a CSS animation has been concluded.

The event may not be fired when e.g. the target element is removed.

Do not be confused with `transitionend`, a similar event for CSS transition.

-}
onAnimationEnd : msg -> Attribute msg
onAnimationEnd msg =
    htmlAttribute (Html.Events.on "animationend" (D.succeed msg))


octiconEl : List (Attribute msg) -> { size : Int, color : Color, shape : Octicons.Options -> Html.Html msg } -> Element msg
octiconEl attrs { size, color, shape } =
    Octicons.defaultOptions
        |> Octicons.color (cssRgba color)
        |> Octicons.size size
        |> shape
        |> html
        |> el attrs


squareIconOrHeadEl : List (Attribute msg) -> { size : Int, name : String, url : Maybe String } -> Element msg
squareIconOrHeadEl userAttrs { size, name, url } =
    let
        baseAttrs =
            [ width (px size)
            , height (px size)
            , clip
            , BG.color iconBackground
            , BD.rounded (iconRounding size)
            , Font.size (size // 2)
            , Font.bold
            , Font.family
                [ Font.typeface "Tahoma"
                , Font.typeface "Verdana"
                , Font.typeface "Arial"
                , Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            ]
    in
    el (baseAttrs ++ userAttrs) <|
        case url of
            Just url_ ->
                image [ width (px size), height (px size), centerX, centerY ] { src = url_, description = name }

            Nothing ->
                el [ centerX, centerY ] (text (String.left 1 name))


iconBackground : Color
iconBackground =
    oneDark.sub


iconRounding : Int -> Int
iconRounding size =
    max 2 (size // 10)


iconWithBadgeEl :
    List (Attribute msg)
    ->
        { size : Int
        , theme : ColorTheme
        , badge : Maybe (Int -> Element msg)
        , fallback : String
        , url : Maybe String
        }
    -> Element msg
iconWithBadgeEl userAttrs opts =
    let
        bottomRightBadgeAttrs =
            case opts.badge of
                Just badgeEl ->
                    let
                        badgeSize =
                            opts.size // 3
                    in
                    [ padding innerIconPadding
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
            max 1 (opts.size // 20)
    in
    el (bottomRightBadgeAttrs ++ userAttrs) <|
        squareIconOrHeadEl [ BG.color opts.theme.prim ]
            { size = opts.size - (innerIconPadding * 2), name = opts.fallback, url = opts.url }


textInputEl :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , theme : ColorTheme
        , enabled : Bool
        , text : String
        , label : Element.Input.Label msg
        , placeholder : Maybe (Element msg)
        }
    -> Element msg
textInputEl attrs { onChange, theme, enabled, text, label, placeholder } =
    let
        baseAttrs =
            [ width fill
            , height fill
            , padding textInputPadding
            , BG.color theme.note
            , BD.width 0
            , BD.rounded rectElementRound
            , Font.color theme.text
            , switchCursor enabled
            , customPlaceholder theme placeholder text
            , inputScreen rectElementRound enabled
            , style "line-height" "1" -- Cancelling line-height introduced by elm-ui
            , disabled (not enabled)
            ]
    in
    Element.Input.text (baseAttrs ++ attrs)
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , label = label
        }


disabled : Bool -> Attribute msg
disabled disabled_ =
    if disabled_ then
        htmlAttribute (Html.Attributes.disabled True)

    else
        noneAttr


textInputPadding : Int
textInputPadding =
    5


customPlaceholder : ColorTheme -> Maybe (Element msg) -> String -> Attribute msg
customPlaceholder theme phMaybe text =
    -- elm-ui's placeholder uses opacity to switch visibility (which should be fast on paper),
    -- but it triggers style recalculation somehow in large SPA (probably due to class swapping and diffing on many DOMs).
    -- Transforming scale/translate directly on specific elements do not trigger style recalculation,
    -- are cheap and fast, and with inFront (position: absolute;), cause no layout/reflow.
    case phMaybe of
        Just ph ->
            inFront <|
                el
                    [ paddingXY textInputPadding 0
                    , centerY
                    , Font.color (setAlpha 0.5 theme.text)
                    , style "transition" "transform 0.3s"
                    , if String.isEmpty text then
                        noneAttr

                      else
                        style "transform" "scale(0.75) translate(-20%, -150%)"
                    ]
                    ph

        Nothing ->
            noneAttr


multilineInputEl :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , key : String
        , placeholder : Maybe String
        , label : Element.Input.Label msg
        , spellcheck : Bool
        , width : Length
        }
    -> Element msg
multilineInputEl userAttrs opts =
    let
        baseAttrs =
            [ padding rectElementInnerPadding
            , BD.width 0
            , style "transition" "height 0.2s 0.1s,background-color 0.3s,color 0.3s"
            , case opts.placeholder of
                Just ph ->
                    htmlAttribute (Html.Attributes.placeholder ph)

                Nothing ->
                    noneAttr
            ]
    in
    Element.Input.multiline (baseAttrs ++ userAttrs)
        { onChange = opts.onChange
        , text = opts.text
        , placeholder = Nothing
        , label = opts.label
        , spellcheck = opts.spellcheck
        }
        -- Workaround for https://github.com/mdgriffith/elm-ui/issues/5
        |> Tuple.pair opts.key
        |> Element.Keyed.el [ width opts.width ]


toggleInputEl :
    List (Attribute msg)
    ->
        { onChange : Bool -> msg
        , height : Int
        , checked : Bool
        }
    -> Element msg
toggleInputEl attrs opts =
    let
        rounding =
            max 1 (opts.height // 5)

        innerHeight =
            opts.height - 2

        baseAttrs =
            [ width (px (opts.height * 2))
            , height (px opts.height)
            , padding 1
            , BD.rounded rounding
            , BG.color oneDark.note
            , behindContent <|
                el
                    [ width (px (opts.height * 2))
                    , height (px opts.height)
                    , BD.rounded rounding
                    , BG.color oneDark.succ
                    , style "transition" "opacity 0.25s"
                    , if opts.checked then
                        style "opacity" "1"

                      else
                        style "opacity" "0"
                    ]
                    none
            ]
    in
    Element.Input.button (baseAttrs ++ attrs)
        { onPress = Just (opts.onChange (not opts.checked))
        , label = lazy3 toggleHandleEl rounding innerHeight opts.checked
        }


toggleHandleEl : Int -> Int -> Bool -> Element msg
toggleHandleEl rounding innerHeight checked =
    el
        [ width (px innerHeight)
        , height (px innerHeight)
        , BD.rounded rounding
        , BG.color oneDark.text
        , alignLeft
        , style "transition" "transform 0.25s"
        , if checked then
            style "transform" ("translateX(" ++ String.fromInt (innerHeight + 1) ++ "px)")

          else
            style "transform" "translateX(0px)"
        ]
        none


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
            , inputScreen rectElementRound enabled
            , switchCursor enabled
            ]
                ++ userAttrs
    in
    Element.Input.button attrs
        { onPress =
            if enabled then
                Just onPress

            else
                Nothing
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
inputScreen : Int -> Bool -> Attribute msg
inputScreen round enabled =
    inFront <|
        el
            [ width fill
            , height fill
            , visible (not enabled)
            , BD.rounded round
            , BG.color (rgba255 0 0 0 0.5)
            ]
            none


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
            , inputScreen thinButtonPadding enabled
            , switchCursor enabled
            ]
                ++ userAttrs
    in
    Element.Input.button attrs
        { onPress =
            if enabled then
                Just onPress

            else
                Nothing
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
    List (Attribute msg)
    ->
        { onPress : msg
        , enabled : Bool
        , innerElement : Element msg
        , innerElementSize : Int
        }
    -> Element msg
roundButtonEl userAttrs { onPress, enabled, innerElement, innerElementSize } =
    let
        baseAttrs =
            [ width (px innerElementSize)
            , height (px innerElementSize)
            , BD.rounded (innerElementSize // 2 + 1)
            , switchCursor enabled
            , roundInputScreen innerElementSize enabled
            , disabled (not enabled)
            ]
    in
    Element.Input.button (baseAttrs ++ userAttrs)
        { onPress =
            if enabled then
                Just onPress

            else
                Nothing
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
        , round : Int
        , innerElement : Element msg
        , innerElementSize : Int
        }
    -> Element msg
squareButtonEl userAttrs { onPress, enabled, round, innerElement, innerElementSize } =
    let
        attrs =
            [ width (px innerElementSize)
            , height (px innerElementSize)
            , clip
            , BD.rounded round
            , switchCursor enabled
            , inputScreen round enabled
            , disabled (not enabled)
            ]
                ++ userAttrs
    in
    Element.Input.button attrs
        { onPress =
            if enabled then
                Just onPress

            else
                Nothing
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
    paragraph <| attrs ++ [ htmlAttribute (Html.Attributes.class breakClassName) ]


breakClassName : String
breakClassName =
    "breakEl"


{-| `textColumn` with "word-break: break-all" and "white-space: pre-wrap".
-}
breakTColumn : List (Attribute msg) -> List (Element msg) -> Element msg
breakTColumn attrs =
    textColumn <| htmlAttribute (Html.Attributes.class breakClassName) :: attrs


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
cssRgba : Color -> String
cssRgba color =
    let
        { red, green, blue, alpha } =
            toRgb color
    in
    String.join ""
        [ "rgba("
        , String.fromFloat (255 * red)
        , ","
        , String.fromFloat (255 * green)
        , ","
        , String.fromFloat (255 * blue)
        , ","
        , String.fromFloat alpha
        , ")"
        ]


dragHandle : Decoder msg -> List (Attribute msg)
dragHandle onDragstart =
    [ htmlAttribute (Html.Attributes.draggable "true")
    , htmlAttribute (Html.Attributes.class dragHandleClassName)
    , htmlAttribute (Html.Events.on "dragstart" onDragstart)
    ]


dragHandleClassName : String
dragHandleClassName =
    "dragHandle"


filtersToIconEl : List (Attribute msg) -> { size : Int, fam : FilterAtomMaterial, filters : Array Filter } -> Element msg
filtersToIconEl attrs { size, fam, filters } =
    filters
        |> Array.foldl (filterToIconEl attrs size fam) Nothing
        |> Maybe.withDefault (lazy2 fallbackIconEl attrs size)


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


filterToIconEl : List (Attribute msg) -> Int -> FilterAtomMaterial -> Filter -> Maybe (Element msg) -> Maybe (Element msg)
filterToIconEl attrs size fam filter elMaybe =
    let
        reducer filterAtom acc =
            case ( acc, filterAtom ) of
                ( Just _, _ ) ->
                    acc

                ( Nothing, OfDiscordChannel cId ) ->
                    withDiscordChannel cId fam (discordChannelIconEl attrs size)

                ( Nothing, OfSlackConversation cId ) ->
                    withSlackConversation cId fam (slackConversationIconEl attrs size)

                ( Nothing, _ ) ->
                    Nothing
    in
    Filter.foldl reducer elMaybe filter


withDiscordChannel : String -> FilterAtomMaterial -> (Discord.ChannelCache -> a) -> Maybe a
withDiscordChannel cId fam mapper =
    fam.ofDiscordChannel
        |> Maybe.andThen (\( _, caches ) -> ListExtra.findOne (\c -> c.id == cId) caches)
        |> Maybe.map mapper


withSlackConversation : String -> FilterAtomMaterial -> (Slack.ConversationCache -> a) -> Maybe a
withSlackConversation cId fam mapper =
    fam.ofSlackConversation
        |> Maybe.andThen (\{ conversations } -> Slack.getConversationFromCache cId conversations)
        |> Maybe.map mapper


discordChannelIconEl : List (Attribute msg) -> Int -> Discord.ChannelCache -> Element msg
discordChannelIconEl attrs size c =
    case c.guildMaybe of
        Just guild ->
            iconWithBadgeEl attrs
                { size = size
                , theme = oneDark
                , badge = Just (lazy discordBadgeEl)
                , fallback = c.name
                , url = Maybe.map (Discord.imageUrlNoFallback (Just size)) guild.icon
                }

        Nothing ->
            iconWithBadgeEl attrs
                { size = size
                , theme = oneDark
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


slackConversationIconEl : List (Attribute msg) -> Int -> Slack.ConversationCache -> Element msg
slackConversationIconEl attrs size c =
    iconWithBadgeEl attrs
        { size = size
        , theme = aubergine
        , badge = Just (lazy2 slackLogoClippedEl [])
        , fallback = c.team.name
        , url = chooseSlackTeamIconUrl size c.team.icon
        }


filtersToTextEl :
    List (Attribute msg)
    ->
        { fontSize : Int
        , color : Color
        , fam : FilterAtomMaterial
        , filters : Array Filter
        }
    -> Element msg
filtersToTextEl attrs { fontSize, color, fam, filters } =
    let
        arrayReducer f acc =
            List.sortWith Filter.compareFilterAtom (Filter.toList f) :: acc
    in
    filters
        |> Array.foldr arrayReducer []
        |> List.concatMap (List.map (filterAtomTextEl fontSize color fam))
        |> List.intersperse (breakT "  ")
        |> breakP attrs


filterAtomTextEl : Int -> Color -> FilterAtomMaterial -> FilterAtom -> Element msg
filterAtomTextEl fontSize color fam fa =
    case fa of
        OfDiscordChannel cId ->
            el [ Font.size fontSize, Font.color color, Font.bold ] <|
                Maybe.withDefault (breakT cId) <|
                    withDiscordChannel cId fam <|
                        \c -> breakT ("#" ++ c.name)

        OfSlackConversation cId ->
            el [ Font.size fontSize, Font.color color, Font.bold ] <|
                Maybe.withDefault (breakT cId) <|
                    withSlackConversation cId fam <|
                        \c ->
                            case c.type_ of
                                Slack.PublicChannel _ ->
                                    breakT ("#" ++ c.name)

                                Slack.PrivateChannel ->
                                    breakT ("#" ++ c.name)

                                _ ->
                                    breakT c.name

        ByMessage query ->
            breakT ("\"" ++ query ++ "\"")

        ByMedia HasImage ->
            octiconEl [] { size = fontSize, color = color, shape = Octicons.fileMedia }

        ByMedia HasVideo ->
            octiconEl [] { size = fontSize, color = color, shape = Octicons.deviceCameraVideo }

        ByMedia HasNone ->
            octiconEl [] { size = fontSize, color = color, shape = Octicons.textSize }

        RemoveMe ->
            none


fetchStatusTextEl : Time.Zone -> FetchStatus -> Element msg
fetchStatusTextEl tz fs =
    breakT <|
        case fs of
            FetchStatus.NextFetchAt posix _ ->
                "Next: " ++ TimeExtra.local tz posix

            FetchStatus.Fetching _ _ ->
                "Fetching..."

            FetchStatus.Waiting ->
                "Checking availability..."

            FetchStatus.InitialFetching _ ->
                "Checking availability..."

            FetchStatus.Available ->
                "Not subscribed"


discordGuildIconEl : List (Attribute msg) -> Int -> Discord.Guild -> Element msg
discordGuildIconEl attrs size guild =
    squareIconOrHeadEl attrs
        { size = size
        , name = guild.name
        , url = Maybe.map (Discord.imageUrlNoFallback (Just size)) guild.icon
        }


discordChannelEl : List (Attribute msg) -> { size : Int, channel : { x | name : String, guildMaybe : Maybe Discord.Guild } } -> Element msg
discordChannelEl attrs { size, channel } =
    row ([ spacing channelTextSpacingX ] ++ attrs)
        [ channel.guildMaybe |> Maybe.map (discordGuildIconEl [] size) |> Maybe.withDefault none
        , text "#"
        , text channel.name
        ]


channelTextSpacingX : Int
channelTextSpacingX =
    2


slackLogoClippedEl : List (Attribute msg) -> Int -> Element msg
slackLogoClippedEl attrs targetSize =
    -- Slack logo returns with transparent outer frame, so we must clip that
    let
        slackIconSize =
            (targetSize * 7) // 5

        translate =
            -- Negative value
            String.fromFloat (toFloat (targetSize - slackIconSize) / 2)
    in
    el
        [ width (px targetSize)
        , height (px targetSize)
        , BD.rounded (iconRounding targetSize)
        , clip
        , behindContent <|
            image [ style "transform" ("translate(" ++ translate ++ "px," ++ translate ++ "px)") ]
                { src = Slack.defaultIconUrl (Just slackIconSize)
                , description = "Slack"
                }
        ]
        none


slackConversationEl :
    List (Attribute msg)
    ->
        { fontSize : Int
        , conversation : { x | name : String, type_ : Slack.ConversationType }
        , team : Maybe ( Slack.Team, Int )
        }
    -> Element msg
slackConversationEl attrs opts =
    row ([ spacing channelTextSpacingX, Font.size opts.fontSize ] ++ attrs) <|
        [ case opts.team of
            Just ( team, size ) ->
                slackTeamIconEl [] size team

            Nothing ->
                none
        , case opts.conversation.type_ of
            Slack.PublicChannel _ ->
                el [ width (px opts.fontSize), Font.center ] (text "#")

            Slack.PrivateChannel ->
                octiconEl [] { size = opts.fontSize, color = slackConvIconColor, shape = Octicons.lock }

            Slack.IM ->
                octiconEl [] { size = opts.fontSize, color = slackConvIconColor, shape = Octicons.person }

            Slack.MPIM ->
                octiconEl [] { size = opts.fontSize, color = slackConvIconColor, shape = Octicons.organization }
        , text opts.conversation.name
        ]


slackConvIconColor : Color
slackConvIconColor =
    aubergine.text


slackTeamIconEl : List (Attribute msg) -> Int -> Slack.Team -> Element msg
slackTeamIconEl attrs size team =
    squareIconOrHeadEl ([ BG.color aubergine.prim ] ++ attrs)
        { size = size
        , name = team.name
        , url = chooseSlackTeamIconUrl size team.icon
        }


chooseSlackTeamIconUrl : Int -> Slack.TeamIcon -> Maybe String
chooseSlackTeamIconUrl size icon =
    if icon.imageDefault then
        Nothing

    else if size <= 34 then
        Just (Url.toString icon.image34)

    else if size <= 44 then
        Just (Url.toString icon.image44)

    else
        Just (Url.toString icon.image68)



-- MANUAL STYLE


manualStyle : Html.Html msg
manualStyle =
    Html.node "style"
        []
        [ Html.text "::-webkit-scrollbar{display:none;}" -- Hidden scrollbars
        , Html.text <| "." ++ breakClassName ++ "{white-space:pre-wrap!important;word-break:break-all!important;}" -- Breakable inline texts
        , Html.text <| "." ++ dragHandleClassName ++ "{cursor:all-scroll;}" -- Drag handle cursor
        , Html.text ":focus{box-shadow:0px 0px 3px 3px rgb(103,123,196);outline:none;}" -- Manual focus style
        , Html.text "a:link{text-decoration:none;}" -- Disabled browser-default link-underlining
        , Html.text "a:link:hover{text-decoration:underline;}" -- Workaround for underline not being appliable to mouseOver or focused
        , Html.text <| "@keyframes " ++ borderFlashKeyframesName ++ "{from{border-color:rgb(220,221,222);}to{border-color:inherit;}}"
        , Html.text <| "@keyframes " ++ rotatingKeyframesName ++ "{from{transform:rotate(0turn);}to{transform:rotate(1turn);}}"
        , Html.text <| "@keyframes " ++ wiggleKeyframesName ++ "{" ++ wiggleKeyframes ++ "}"
        ]


wiggleKeyframes : String
wiggleKeyframes =
    String.join ""
        [ "0%{transform:rotate(10deg);}"
        , "25%{transform:rotate(-10deg);}"
        , "50%{transform:rotate(20deg);}"
        , "75%{transform:rotate(-5deg);}"
        , "100%{transform:rotate(0deg);}"
        ]



-- CONSTANTS


columnWidth : Int
columnWidth =
    350


columnHeaderHeight : Int
columnHeaderHeight =
    columnHeaderIconSize + rectElementInnerPadding


columnHeaderIconSize : Int
columnHeaderIconSize =
    32


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


columnItemMinimumHeight : Int
columnItemMinimumHeight =
    columnItemAvatarSize + rectElementInnerPadding + columnItemBorderBottom


columnItemAvatarSize : Int
columnItemAvatarSize =
    40


columnItemBorderBottom : Int
columnItemBorderBottom =
    2


columnPinColor : Color
columnPinColor =
    oneDark.warn


columnBorderWidth : Int
columnBorderWidth =
    2


{-| Octicons.trashcan is slllllightly leaning right. Adjusting with this paddingEach.
-}
trashcanPaddingAdjust : { top : Int, right : Int, bottom : Int, left : Int }
trashcanPaddingAdjust =
    { top = 0, right = 2, bottom = 0, left = 0 }
