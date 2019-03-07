module TextParser exposing
    ( Parsed, ParseOptions, map, apply, parse, parseOptions, defaultOptions
    , shortenUrl
    )

{-| Parses various marked-up texts into intermediate representaitons (IR)
which can then be fed into View modules for actual rendering.

Primarily it exists for parsing Markdown texts,
but it also parses other service-specific markups.

Using pure-Elm Markdown parser: <https://package.elm-lang.org/packages/pablohirafuji/elm-markdown/latest>
Therefore it is slower than other Markdown parser solutions,
so you should consider parsing only once and storing `Parsed` IR for later uses.

@docs Parsed, ParseOptions, map, apply, parse, parseOptions, defaultOptions
@docs shortenUrl

-}

import Markdown.Block as Block exposing (Block(..))
import Markdown.Config
import Markdown.Inline exposing (Inline(..))
import Parser exposing ((|.), (|=), Parser, Step(..))
import Url exposing (Url)


{-| Intermediate representation of parsed texts.

Can be persisted for later uses.

Its internal data structure is Markdown.Block with removing
"original text" from items with children (in order to reduce storage usage).

-}
type Parsed
    = Parsed (List (Block () ()))


map : (Block () () -> a) -> Parsed -> List a
map mapper (Parsed blocks) =
    List.map mapper blocks


{-| Used from TextRenderer; to be removed after Atomic Design Migration.
-}
apply : (List (Block () ()) -> a) -> Parsed -> a
apply renderer (Parsed blocks) =
    renderer blocks


{-| Parse options.

  - `markdown` : Parse text as Markdown. Parser applied AFTER `preFormat` funciton
  - `autoLink` : Apply `autoLinker` at the END of the process, linkifying not-yet-linkified URLs appearing in `Text` nodes.
  - `unescapeTags` : Unescape `&lt;` and `&gt;` to `<` and `>`, allowing them to be parsed by Markdown parser (as HTML tags.)
    Even though they are unescaped, potentially unsafe tags such as `<script>` are not parsed by subsequent parsers.
  - `preFormat` : A function when your texts need to be pre-processed before parsed by markdown parser or `autoLinker`.
    E.g. Resolve Slack's message formatting into proper Markdowns.
  - `customInlineFormat` : A function when you need to apply additional funciton to once-parsed Inline nodes.

-}
type alias ParseOptions =
    { markdown : Bool
    , autoLink : Bool
    , unescapeTags : Bool
    , preFormat : Maybe (String -> String)
    , customInlineFormat : Maybe (Inline () -> Inline ())
    }


parseOptions : ParseOptions
parseOptions =
    { markdown = False
    , autoLink = False
    , unescapeTags = False
    , preFormat = Nothing
    , customInlineFormat = Nothing
    }


defaultOptions : ParseOptions
defaultOptions =
    { parseOptions | markdown = True, autoLink = True }


parse : ParseOptions -> String -> Parsed
parse opts raw =
    let
        unescapeTags =
            if opts.unescapeTags then
                unescapeAngles

            else
                identity

        parseAsMarkdown text =
            if opts.markdown then
                Block.parse blockParseOptions text

            else
                [ Paragraph "" [ Text text ] ]
    in
    Maybe.withDefault raw (Maybe.map ((|>) raw) opts.preFormat)
        |> unescapeTags
        |> parseAsMarkdown
        |> List.map (Block.walk (afterWalkBlock opts))
        |> Parsed


blockParseOptions : Maybe Markdown.Config.Options
blockParseOptions =
    let
        -- Apply custom HTML sanitization here
        default =
            Markdown.Config.defaultOptions

        sanitizeOpions =
            { allowedHtmlElements = [ "code" ]
            , allowedHtmlAttributes = []
            }
    in
    Just { default | rawHtml = Markdown.Config.Sanitize sanitizeOpions }



-- Angles Unescaper


unescapeAngles : String -> String
unescapeAngles raw =
    let
        parser =
            Parser.loop [] <|
                \acc ->
                    Parser.oneOf
                        [ Parser.end |> Parser.map (\() -> Parser.Done (List.foldl (++) "" acc))
                        , ampedStringParser |> Parser.map (\s -> Parser.Loop (s :: acc))
                        , Parser.chompUntilEndOr "&" |> Parser.mapChompedString (\chomped () -> Parser.Loop (chomped :: acc))
                        ]

        ampedStringParser =
            Parser.oneOf
                [ Parser.token "&gt;" |> Parser.map (always ">")
                , Parser.token "&lt;" |> Parser.map (always "<")
                , Parser.chompIf ((==) '&')
                    |. Parser.chompUntilEndOr "&"
                    |> Parser.mapChompedString (\chomped () -> chomped)
                ]
    in
    Parser.run parser raw |> Result.withDefault raw


afterWalkBlock : ParseOptions -> Block () () -> Block () ()
afterWalkBlock opts block =
    -- Also removing raw texts stored in parent nodes
    case block of
        Heading _ level inlines ->
            Heading "" level <| autoLinker opts.autoLink <| applyCustomFormat opts.customInlineFormat inlines

        Paragraph _ inlines ->
            Paragraph "" <| autoLinker opts.autoLink <| applyCustomFormat opts.customInlineFormat inlines

        PlainInlines inlines ->
            PlainInlines <| autoLinker opts.autoLink <| applyCustomFormat opts.customInlineFormat inlines

        _ ->
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

NOTE Tests had shown this function is not so performant ;<
Better not use with Markdown.

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

        (Text "") :: is ->
            autoLinkerImpl is (Text "" :: acc)

        (Text raw) :: is ->
            case Parser.run parseIntoTextOrLinks raw of
                Ok linkified ->
                    -- linkified Inline nodes are "properly" ordered; must reverse before adding to acc
                    autoLinkerImpl is (List.reverse linkified ++ acc)

                Err _ ->
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
