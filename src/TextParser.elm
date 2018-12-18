module TextParser exposing
    ( Parsed(..), ParseOptions, parse, defaultOptions
    , shortenUrl
    )

{-| Parses various marked-up texts into intermediate representaitons (IR)
which can then be fed into TextRenderer for actual rendering.

Primarily it exists for parsing Markdown texts,
but it also parses other service-specific markups.

Using pure-Elm Markdown parser: <https://package.elm-lang.org/packages/pablohirafuji/elm-markdown/latest>
Therefore it is slower than other Markdown parser solutions,
so you should consider parsing only once and storing `Parsed` IR for later uses.

@docs Parsed, ParseOptions, parse, defaultOptions
@docs shortenUrl

-}

import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))
import Parser exposing ((|.), (|=), Parser, Step(..))
import Url exposing (Url)


{-| IR of parsed texts. Meant to be persisted for later uses.

Its internal data structure is Markdown.Block with removing
"original text" from items with children (in order to reduce IndexedDB usage).

-}
type Parsed
    = Parsed (List (Block () ()))


{-| Parse options.

Use `preFormat` function when your texts need to be pre-processed
before parsed by markdown parser or autoLinker.
E.g. Resolve Slack's message formatting into proper Markdowns.

Use `customInlineFormat` function when you need to apply additional funciton to once-parsed Inline nodes.

Use `autoLink` to apply autoLinker at the end of the process.
It linkifies not-yet-linkified URLs appearing in Text nodes.

-}
type alias ParseOptions =
    { markdown : Bool
    , autoLink : Bool
    , preFormat : Maybe (String -> String)
    , customInlineFormat : Maybe (Inline () -> Inline ())
    }


defaultOptions : ParseOptions
defaultOptions =
    { markdown = True
    , autoLink = True
    , preFormat = Nothing
    , customInlineFormat = Nothing
    }


parse : ParseOptions -> String -> Parsed
parse opts raw =
    let
        parseAsMarkdown text =
            if opts.markdown then
                Block.parse Nothing text

            else
                [ Paragraph "" [ Text text ] ]
    in
    Parsed <|
        List.map (Block.walk (afterWalkBlock opts)) <|
            parseAsMarkdown <|
                Maybe.withDefault raw (Maybe.map ((|>) raw) opts.preFormat)


afterWalkBlock : ParseOptions -> Block () () -> Block () ()
afterWalkBlock opts block =
    -- Also removing raw texts stored in parent nodes
    case block of
        Heading raw level inlines ->
            Heading "" level <| autoLinker opts.autoLink <| applyCustomFormat opts.customInlineFormat inlines

        Paragraph raw inlines ->
            Paragraph "" <| autoLinker opts.autoLink <| applyCustomFormat opts.customInlineFormat inlines

        PlainInlines inlines ->
            PlainInlines <| autoLinker opts.autoLink <| applyCustomFormat opts.customInlineFormat inlines

        _ ->
            -- XXX check if walk reaches child nodes; doc says Block.walk "recursively" applies a function to blocks
            block


applyCustomFormat : Maybe (Inline () -> Inline ()) -> List (Inline ()) -> List (Inline ())
applyCustomFormat customFormat inlines =
    case customFormat of
        Just func ->
            List.map func inlines

        Nothing ->
            inlines



-- Autolink Parser


{-| Convert URLs appearing in not-yet-linkified Text nodes.
-}
autoLinker : Bool -> List (Inline ()) -> List (Inline ())
autoLinker enabled inlines =
    if enabled then
        autoLinkerImpl inlines []

    else
        inlines


autoLinkerImpl : List (Inline ()) -> List (Inline ()) -> List (Inline ())
autoLinkerImpl inlines acc =
    case inlines of
        [] ->
            List.foldl compactify [] acc

        (Text raw) :: is ->
            case Parser.run parseIntoTextOrLinks raw of
                Ok linkified ->
                    -- linkified Inline nodes are "properly" ordered; must reverse before adding to acc
                    autoLinkerImpl is (List.reverse linkified ++ acc)

                Err e ->
                    -- Debug here
                    autoLinkerImpl is (Text raw :: acc)

        ((Link _ _ _) as i) :: is ->
            -- Already linkified, skip
            autoLinkerImpl is (i :: acc)

        ((Image _ _ _) as i) :: is ->
            -- Children of Image node is placeholder; skip
            autoLinkerImpl is (i :: acc)

        (Emphasis level children) :: is ->
            let
                autoLinkedChildren =
                    autoLinkerImpl children []
            in
            autoLinkerImpl is (Emphasis level autoLinkedChildren :: acc)

        (HtmlInline tag attrs children) :: is ->
            let
                autoLinkedChildren =
                    autoLinkerImpl children []
            in
            autoLinkerImpl is (HtmlInline tag attrs autoLinkedChildren :: acc)

        i :: is ->
            -- Otherwise skip; we are not using Custom nodes
            autoLinkerImpl is (i :: acc)


parseIntoTextOrLinks : Parser (List (Inline ()))
parseIntoTextOrLinks =
    -- Accumulate from left to right, then popping accumulator while applying compactify, thus restoring the original order
    Parser.loop [] <|
        \acc ->
            Parser.oneOf
                [ Parser.end |> Parser.map (\_ -> Done (List.foldl compactify [] acc))
                , chompUrlLike |> Parser.map (\textOrLink -> Loop (textOrLink :: acc))
                , chompNonUrl |> Parser.map (\str -> Loop (Text str :: acc))
                ]


compactify : Inline () -> List (Inline ()) -> List (Inline ())
compactify elem acc =
    case acc of
        (Text x) :: xs ->
            case elem of
                Text y ->
                    -- Concat neighboring Text nodes
                    Text (y ++ x) :: xs

                _ ->
                    elem :: acc

        _ ->
            elem :: acc


chompUrlLike : Parser (Inline ())
chompUrlLike =
    let
        parseAsUrl nonTerminatorStr =
            let
                urlCandidate =
                    "http" ++ nonTerminatorStr
            in
            case Url.fromString urlCandidate of
                Just url ->
                    Link urlCandidate Nothing [ Text (shortenUrl url) ]

                Nothing ->
                    -- Restoring original text which started with "http" but not a valid URL
                    Text ("http" ++ nonTerminatorStr)
    in
    Parser.succeed parseAsUrl
        |. Parser.token "http"
        |= Parser.getChompedString (Parser.chompWhile nonTerminator)


shortenUrl : Url -> String
shortenUrl url =
    let
        shortUrl =
            url.host ++ url.path
    in
    if String.endsWith "/" shortUrl then
        String.dropRight 1 shortUrl

    else
        shortUrl


chompNonUrl : Parser String
chompNonUrl =
    Parser.getChompedString <| Parser.chompUntilEndOr "http"


nonTerminator : Char -> Bool
nonTerminator c =
    not <|
        List.member c
            [ ' '
            , '\t'
            , '\n'
            , '\u{000D}'
            , '|' -- Introduced as a temporary measure until we support proper formatting for Slack
            , '>' -- Introduced as a temporary measure until we support proper formatting for Slack
            ]
