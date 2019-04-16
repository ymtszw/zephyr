module Data.Producer.Slack.Message.AngleCmd exposing (resolve)

import AssocList exposing (Dict)
import Data.Producer.Slack.Convo as Convo exposing (Convo)
import Data.Producer.Slack.User as User exposing (User)
import Id
import Parser exposing ((|.), (|=), Parser)
import StringExtra
import Url exposing (Url)


{-| Convert special message formatting syntax into proper markdown.

  - Converts `<...>` special syntax into markdown (or plain text)
  - Resolves User/Channel ID to readable names

At first this was meant to be executed as preFormat of TextParser,
but instead, it should be used on message fetches.

-}
resolve : Dict Convo.Id Convo -> Dict User.Id User -> String -> String
resolve convos users raw =
    let
        resolveByLine line =
            case Parser.run (angleSyntaxParser convos users) line of
                Ok replaced ->
                    replaced

                Err _ ->
                    -- Debug here
                    line
    in
    -- Slack text may be truncated. In such cases angles may NOT be closed, causing parse failure.
    -- We can introduce many `backtrackable`, but instead we go easy by just localizing failures within lines.
    String.split "\n" raw
        |> List.map resolveByLine
        |> String.join "\n"


angleSyntaxParser : Dict Convo.Id Convo -> Dict User.Id User -> Parser String
angleSyntaxParser convos users =
    Parser.loop [] <|
        \acc ->
            let
                goNext str =
                    Parser.Loop (str :: acc)
            in
            Parser.oneOf
                [ Parser.end |> Parser.map (\() -> Parser.Done (List.foldl (++) "" acc))
                , angleCmdParser |> Parser.map (convertAngleCmd convos users) |> Parser.map goNext
                , Parser.chompUntilEndOr "<" |> Parser.getChompedString |> Parser.map goNext
                ]


type AngleCmd
    = AtUser String (Maybe String)
    | AtEveryone
    | AtHere
    | AtChannel
    | OtherSpecial String (Maybe String)
    | ToChannel String (Maybe String)
    | Link Url (Maybe String)


angleCmdParser : Parser AngleCmd
angleCmdParser =
    let
        urlParser str =
            case Url.fromString str of
                Just url ->
                    Parser.succeed url

                Nothing ->
                    Parser.problem ("Not a valid URL: " ++ str)
    in
    Parser.succeed identity
        |. Parser.symbol "<"
        |= Parser.oneOf
            [ Parser.succeed AtUser
                |. Parser.symbol "@"
                |= rawKeywordParser
                |= remainderParser
            , Parser.succeed identity
                |. Parser.symbol "!"
                |= Parser.oneOf
                    [ Parser.succeed AtEveryone
                        |. Parser.keyword "everyone"
                        |. remainderParser
                    , Parser.succeed AtHere
                        |. Parser.keyword "here"
                        |. remainderParser
                    , Parser.succeed AtChannel
                        |. Parser.keyword "channel"
                        |. remainderParser
                    , -- XXX e.g. Date syntax
                      Parser.succeed OtherSpecial
                        |= rawKeywordParser
                        |= remainderParser
                    ]
            , Parser.succeed ToChannel
                |. Parser.symbol "#"
                |= rawKeywordParser
                |= remainderParser
            , Parser.succeed Link
                |= (rawKeywordParser |> Parser.andThen urlParser)
                |= remainderParser
            ]
        |. Parser.symbol ">"


rawKeywordParser : Parser String
rawKeywordParser =
    Parser.chompWhile (\c -> c /= '|' && c /= '>')
        |> Parser.getChompedString


remainderParser : Parser (Maybe String)
remainderParser =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "|"
            |= Parser.getChompedString (Parser.chompWhile ((/=) '>'))
        , Parser.succeed Nothing
        ]


convertAngleCmd : Dict Convo.Id Convo -> Dict User.Id User -> AngleCmd -> String
convertAngleCmd convos users angleCmd =
    let
        placeholderOr prefix ph func =
            case ph of
                Just str ->
                    prefix ++ str

                Nothing ->
                    prefix ++ func ()
    in
    case angleCmd of
        AtUser userIdStr ph ->
            -- XXX Link?
            placeholderOr "@" ph <|
                \() ->
                    User.resolveUserName users (Id.from userIdStr)

        AtEveryone ->
            "@everyone"

        AtHere ->
            "@here"

        AtChannel ->
            "@channel"

        OtherSpecial str ph ->
            Maybe.withDefault str ph

        ToChannel convoIdStr ph ->
            -- XXX Link?
            placeholderOr "#" ph <|
                \() ->
                    Convo.resolveConvoName convos (Id.from convoIdStr)

        Link url ph ->
            "[" ++ (placeholderOr "" ph <| \() -> StringExtra.fromUrlShortened url) ++ "](" ++ Url.toString url ++ ")"
