module Examples exposing (suite)

import Array exposing (fromList)
import ArrayExtra as Array
import AssocList as Dict
import ColorExtra
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord.Message
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Data.Producer.Slack.Bot as SlackBot
import Data.Producer.Slack.Convo as SlackConvo exposing (Type(..))
import Data.Producer.Slack.Message as SlackMessage
import Data.Producer.Slack.Message.AngleCmd as AngleCmd
import Data.Producer.Slack.Team as SlackTeam
import Data.Producer.Slack.User as SlackUser
import Expect exposing (Expectation)
import Fuzz
import Hex
import Id
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Json.EncodeExtra as E
import Markdown.Block exposing (..)
import Markdown.Inline exposing (..)
import Parser
import SelectArray
import SlackTestData
import String exposing (fromInt, toInt)
import StringExtra
import Test exposing (..)
import TextParser exposing (parseOptions)
import Time exposing (Posix)
import TimeExtra exposing (po)


suite : Test
suite =
    describe "test"
        [ selectArraySuite
        , stringSuite
        , arraySuite
        , textParserSuite
        , filterSuite
        , fetchStatusSuite
        , discordSuite
        , slackSuite
        ]



-- SelectArray


selectArraySuite : Test
selectArraySuite =
    describe "SelectArray"
        [ describe "selectAt"
            [ testSelectAt ( [], 0, [ 1, 2 ] ) 0 ( [], 0, [ 1, 2 ] )
            , testSelectAt ( [], 0, [ 1, 2 ] ) 1 ( [ 0 ], 1, [ 2 ] )
            , testSelectAt ( [], 0, [ 1, 2 ] ) 2 ( [ 0, 1 ], 2, [] )
            , testSelectAt ( [], 0, [ 1, 2 ] ) 3 ( [], 0, [ 1, 2 ] )
            , testSelectAt ( [], 0, [ 1, 2 ] ) -1 ( [], 0, [ 1, 2 ] )
            , testSelectAt ( [ 0 ], 1, [ 2 ] ) 0 ( [], 0, [ 1, 2 ] )
            , testSelectAt ( [ 0 ], 1, [ 2 ] ) 1 ( [ 0 ], 1, [ 2 ] )
            , testSelectAt ( [ 0 ], 1, [ 2 ] ) 2 ( [ 0, 1 ], 2, [] )
            , testSelectAt ( [ 0 ], 1, [ 2 ] ) 3 ( [ 0 ], 1, [ 2 ] )
            , testSelectAt ( [ 0 ], 1, [ 2 ] ) -1 ( [ 0 ], 1, [ 2 ] )
            , testSelectAt ( [ 0, 1 ], 2, [] ) 0 ( [], 0, [ 1, 2 ] )
            , testSelectAt ( [ 0, 1 ], 2, [] ) 1 ( [ 0 ], 1, [ 2 ] )
            , testSelectAt ( [ 0, 1 ], 2, [] ) 2 ( [ 0, 1 ], 2, [] )
            , testSelectAt ( [ 0, 1 ], 2, [] ) 3 ( [ 0, 1 ], 2, [] )
            , testSelectAt ( [ 0, 1 ], 2, [] ) -1 ( [ 0, 1 ], 2, [] )
            ]
        , describe "indexedMap"
            [ testIndexedMap ( [], 0, [ 1, 2 ] ) [ ( 0, True ), ( 1, False ), ( 2, False ) ]
            , testIndexedMap ( [ 0 ], 1, [ 2 ] ) [ ( 0, False ), ( 1, True ), ( 2, False ) ]
            , testIndexedMap ( [ 0, 1 ], 2, [] ) [ ( 0, False ), ( 1, False ), ( 2, True ) ]
            ]
        ]


testSelectAt : ( List a, a, List a ) -> Int -> ( List a, a, List a ) -> Test
testSelectAt ( initialF, initialS, initialR ) index ( expectedF, expectedS, expectedR ) =
    let
        initial =
            SelectArray.fromLists initialF initialS initialR
    in
    test ("should work with: " ++ Debug.toString initial ++ ", index" ++ String.fromInt index) <|
        \_ ->
            initial |> SelectArray.selectAt index |> Expect.equal (SelectArray.fromLists expectedF expectedS expectedR)


testIndexedMap : ( List a, a, List a ) -> List ( Int, Bool ) -> Test
testIndexedMap ( initialF, initialS, initialR ) expected =
    let
        initial =
            SelectArray.fromLists initialF initialS initialR
    in
    test ("should work with: " ++ Debug.toString initial) <|
        \_ ->
            initial |> SelectArray.indexedMap (\{ selected, index } -> ( index, selected )) |> Expect.equal expected



-- StringExtra


stringSuite : Test
stringSuite =
    describe "StringExtra"
        [ describe "splitAt"
            [ testStringSplitAt "initial" 0 [ "initial" ]
            , testStringSplitAt "initial" 1 [ "i", "nitial" ]
            , testStringSplitAt "initial" 2 [ "in", "itial" ]
            , testStringSplitAt "initial" 3 [ "ini", "tial" ]
            , testStringSplitAt "initial" 4 [ "init", "ial" ]
            , testStringSplitAt "initial" 5 [ "initi", "al" ]
            , testStringSplitAt "initial" 6 [ "initia", "l" ]
            , testStringSplitAt "initial" 7 [ "initial" ]
            , testStringSplitAt "initial" -1 [ "initial" ]
            , testStringSplitAt "" 0 [ "" ]
            ]
        , describe "punctuateNumber"
            [ testPunctuateNumber 1 "1"
            , testPunctuateNumber 12 "12"
            , testPunctuateNumber 123 "123"
            , testPunctuateNumber 1234 "1,234"
            , testPunctuateNumber 12345 "12,345"
            , testPunctuateNumber 123456 "123,456"
            , testPunctuateNumber 1234567 "1,234,567"
            ]
        ]


testStringSplitAt : String -> Int -> List String -> Test
testStringSplitAt initial index expected =
    test ("should split '" ++ initial ++ "' at: " ++ fromInt index) <|
        \_ ->
            initial |> StringExtra.splitAt index |> Expect.equal expected


testPunctuateNumber : Int -> String -> Test
testPunctuateNumber initial expected =
    test ("should punctuate '" ++ String.fromInt initial ++ "' to " ++ expected) <|
        \_ ->
            initial |> StringExtra.punctuateNumber |> Expect.equal expected



-- ArrayExtra


arraySuite : Test
arraySuite =
    let
        isEven i =
            modBy 2 i == 0
    in
    describe "ArrayExtra"
        [ describe "splitAt"
            [ testArraySplitAt [ 0, 1, 2 ] 0 ( [], [ 0, 1, 2 ] )
            , testArraySplitAt [ 0, 1, 2 ] 1 ( [ 0 ], [ 1, 2 ] )
            , testArraySplitAt [ 0, 1, 2 ] 2 ( [ 0, 1 ], [ 2 ] )
            , testArraySplitAt [ 0, 1, 2 ] 3 ( [ 0, 1, 2 ], [] )
            , testArraySplitAt [ 0, 1, 2 ] 4 ( [ 0, 1, 2 ], [] )
            , testArraySplitAt [ 0, 1, 2 ] -1 ( [ 0, 1 ], [ 2 ] )
            , testArraySplitAt [ 0, 1, 2 ] -2 ( [ 0 ], [ 1, 2 ] )
            , testArraySplitAt [ 0, 1, 2 ] -3 ( [], [ 0, 1, 2 ] )
            , testArraySplitAt [ 0, 1, 2 ] -4 ( [], [ 0, 1, 2 ] )
            , testArraySplitAt [] 0 ( [], [] )
            , testArraySplitAt [] 1 ( [], [] )
            , testArraySplitAt [] -1 ( [], [] )
            ]
        , describe "removeAt"
            [ testRemoveAt [ 0, 1, 2 ] 0 [ 1, 2 ]
            , testRemoveAt [ 0, 1, 2 ] 1 [ 0, 2 ]
            , testRemoveAt [ 0, 1, 2 ] 2 [ 0, 1 ]
            , testRemoveAt [ 0, 1, 2 ] 3 [ 0, 1, 2 ]
            , testRemoveAt [ 0, 1, 2 ] 4 [ 0, 1, 2 ]
            , testRemoveAt [ 0, 1, 2 ] -1 [ 0, 1 ]
            , testRemoveAt [ 0, 1, 2 ] -2 [ 0, 2 ]
            , testRemoveAt [ 0, 1, 2 ] -3 [ 1, 2 ]
            , testRemoveAt [ 0, 1, 2 ] -4 [ 0, 1, 2 ]
            , testRemoveAt [] 0 []
            , testRemoveAt [] 1 []
            , testRemoveAt [] -1 []
            ]
        , describe "moveFromTo"
            [ testMoveFromTo [ 0, 1, 2, 3 ] 0 0 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 1 1 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 3 3 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 4 4 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 0 1 [ 1, 0, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 0 2 [ 1, 2, 0, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 0 3 [ 1, 2, 3, 0 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 0 4 [ 1, 2, 3, 0 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 1 2 [ 0, 2, 1, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 1 3 [ 0, 2, 3, 1 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 3 4 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 1 0 [ 1, 0, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 1 -1 [ 1, 0, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 2 1 [ 0, 2, 1, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 2 0 [ 2, 0, 1, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 2 -1 [ 2, 0, 1, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] -1 0 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] -1 1 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] -1 3 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] -1 4 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 4 0 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 4 1 [ 0, 1, 2, 3 ]
            , testMoveFromTo [ 0, 1, 2, 3 ] 4 3 [ 0, 1, 2, 3 ]
            ]
        , describe "squeeze"
            [ testSqueeze [ 0, 1, 2, 3 ] 0 9 [ 9, 0, 1, 2, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] 1 9 [ 0, 9, 1, 2, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] 2 9 [ 0, 1, 9, 2, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] 3 9 [ 0, 1, 2, 9, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] 4 9 [ 0, 1, 2, 3, 9 ]
            , testSqueeze [ 0, 1, 2, 3 ] 5 9 [ 0, 1, 2, 3, 9 ]
            , testSqueeze [ 0, 1, 2, 3 ] -1 9 [ 0, 1, 2, 9, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] -2 9 [ 0, 1, 9, 2, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] -3 9 [ 0, 9, 1, 2, 3 ]
            , testSqueeze [ 0, 1, 2, 3 ] -4 9 [ 9, 0, 1, 2, 3 ]
            ]
        , describe "all"
            [ testAll [ 0, 2, 4 ] isEven True
            , testAll [ 0 ] isEven True
            , testAll [] isEven True
            , testAll [ 0, 1, 2 ] isEven False
            ]
        , describe "any"
            [ testAny [ 0, 1, 2 ] isEven True
            , testAny [ 1, 2 ] isEven True
            , testAny [ 1 ] isEven False
            , testAny [] isEven False
            ]
        , describe "member"
            [ testMember [ 0, 1, 2 ] 1 True
            , testMember [ 1, 2 ] 1 True
            , testMember [ 1 ] 1 True
            , testMember [] 1 False
            ]
        , describe "findIndex"
            [ testFindIndex [ 0, 1, 2 ] 0 (Just 0)
            , testFindIndex [ 0, 1, 2 ] 1 (Just 1)
            , testFindIndex [ 0, 1, 2 ] 2 (Just 2)
            , testFindIndex [ 0, 1, 2 ] 3 Nothing
            , testFindIndex [] 0 Nothing
            ]
        ]


testArraySplitAt : List a -> Int -> ( List a, List a ) -> Test
testArraySplitAt initial index ( expectFront, expectRear ) =
    test ("should work with size:" ++ fromInt (List.length initial) ++ ", index:" ++ fromInt index) <|
        \_ ->
            fromList initial
                |> Array.splitAt index
                |> Expect.equal ( fromList expectFront, fromList expectRear )


testRemoveAt : List a -> Int -> List a -> Test
testRemoveAt initial index expect =
    test ("should work with size:" ++ fromInt (List.length initial) ++ ", index: " ++ fromInt index) <|
        \_ ->
            fromList initial
                |> Array.removeAt index
                |> Expect.equal (fromList expect)


testMoveFromTo : List a -> Int -> Int -> List a -> Test
testMoveFromTo initial from to expect =
    test ("should work with size:" ++ fromInt (List.length initial) ++ ", from:" ++ fromInt from ++ ", to:" ++ fromInt to) <|
        \_ ->
            fromList initial
                |> Array.moveFromTo from to
                |> Expect.equal (fromList expect)


testSqueeze : List a -> Int -> a -> List a -> Test
testSqueeze initial index element expected =
    test ("should work with size:" ++ fromInt (List.length initial) ++ ", index:" ++ fromInt index) <|
        \_ ->
            fromList initial
                |> Array.squeeze index element
                |> Expect.equal (fromList expected)


testAll : List a -> (a -> Bool) -> Bool -> Test
testAll initial check expected =
    test ("should work with Array: " ++ Debug.toString initial) <|
        \_ ->
            fromList initial
                |> Array.all check
                |> Expect.equal expected


testAny : List a -> (a -> Bool) -> Bool -> Test
testAny initial check expected =
    test ("should work with Array: " ++ Debug.toString initial) <|
        \_ ->
            fromList initial
                |> Array.any check
                |> Expect.equal expected


testMember : List a -> a -> Bool -> Test
testMember initial item expected =
    test ("should work with Array: " ++ Debug.toString initial) <|
        \_ ->
            fromList initial
                |> Array.member item
                |> Expect.equal expected


testFindIndex : List a -> a -> Maybe Int -> Test
testFindIndex initial item expected =
    test ("should work with Array: " ++ Debug.toString initial ++ ", target: " ++ Debug.toString item) <|
        \_ ->
            fromList initial
                |> Array.findIndex ((==) item)
                |> Expect.equal expected



-- TextParser


textParserSuite : Test
textParserSuite =
    describe "TextParser"
        [ defaultParseSuite
        , autoLinkerSuite
        , unescapeTagsSuite
        ]


defaultParseSuite : Test
defaultParseSuite =
    describe "parse"
        [ testDefaultParse "" [ BlankLine "" ]
        , testDefaultParse "plain text" [ Paragraph "" [ Text "plain text" ] ]
        , testDefaultParse "*marked* __up__ `plain` ~text~"
            [ Paragraph ""
                [ Emphasis 1 [ Text "marked" ]
                , Text " "
                , Emphasis 2 [ Text "up" ]
                , Text " "
                , CodeInline "plain"
                , Text " ~text~"
                ]
            ]
        , testDefaultParse """
# Heading 1
Some texts. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
```lang
type Fenced = Fenced Code
```

After blank line.
Soft line break and [Link](https://example.com "example.com").
"""
            [ BlankLine ""
            , Heading "" 1 [ Text "Heading 1" ]
            , Paragraph "" [ Text "Some texts. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua." ]
            , CodeBlock (Fenced False { fenceChar = "`", fenceLength = 3, indentLength = 0, language = Just "lang" }) "type Fenced = Fenced Code\n"
            , BlankLine ""
            , Paragraph ""
                [ Text "After blank line."
                , HardLineBreak
                , Text "Soft line break and "
                , Link "https://example.com" (Just "example.com") [ Text "Link" ]
                , Text "."
                ]
            , BlankLine ""
            ]
        , testDefaultParse "Plain link without markup. https://example.com"
            [ Paragraph ""
                [ Text "Plain link without markup. "
                , Link "https://example.com" Nothing [ Text "example.com" ]
                ]
            ]
        ]


testDefaultParse : String -> List (Block () ()) -> Test
testDefaultParse initial expected =
    test ("should parse: " ++ initial ++ "") <|
        \_ ->
            TextParser.parse TextParser.defaultOptions initial
                |> equalParsed expected


equalParsed : List (Block () ()) -> TextParser.Parsed -> Expect.Expectation
equalParsed expected parsed =
    Expect.equalLists expected (TextParser.map identity parsed)


testSlackParse : String -> List (Block () ()) -> Test
testSlackParse initial expected =
    test ("should parse: " ++ initial ++ "") <|
        \_ ->
            TextParser.parse SlackMessage.parseOptions initial
                |> equalParsed expected


autoLinkerSuite : Test
autoLinkerSuite =
    let
        s =
            Text

        l urlStr title =
            Link urlStr Nothing [ Text title ]
    in
    describe "autoLinker"
        [ testAutoLinker "" [ s "" ]
        , testAutoLinker " " [ s " " ]
        , testAutoLinker "foobar" [ s "foobar" ]
        , testAutoLinker " very long text with\t\n spaces in it " [ s " very long text with\t\n spaces in it " ]
        , testAutoLinker "text with unparsable http" [ s "text with unparsable http" ]
        , testAutoLinker "text with unparsable http http" [ s "text with unparsable http http" ]
        , testAutoLinker "http://example.com" [ l "http://example.com" "example.com" ]
        , testAutoLinker "http://example.com http://example.com"
            [ l "http://example.com" "example.com", s " ", l "http://example.com" "example.com" ]
        , testAutoLinker "text with parsable http://example.com"
            [ s "text with parsable ", l "http://example.com" "example.com" ]
        , testAutoLinker "textdirectlyfollowedbyhttp://example.com"
            [ s "textdirectlyfollowedby", l "http://example.com" "example.com" ]
        , testAutoLinker "textdirectlyfollowedbyhttp://example.comandnotdelimittedproperly"
            [ s "textdirectlyfollowedby"
            , l "http://example.comandnotdelimittedproperly" "example.comandnotdelimittedproperly"
            ]
        , testAutoLinker "text with parsable http://example.com !"
            [ s "text with parsable ", l "http://example.com" "example.com", s " !" ]
        , testAutoLinker "text with parsable http://example.com http://example.com !"
            [ s "text with parsable "
            , l "http://example.com" "example.com"
            , s " "
            , l "http://example.com" "example.com"
            , s " !"
            ]
        , testAutoLinker "text with parsable http://example.com and unparsable http !"
            [ s "text with parsable ", l "http://example.com" "example.com", s " and unparsable http !" ]
        , testAutoLinker "マルチバイト文字及びマルチバイトURLを含む http://example.com http://マルチバイト.jp"
            [ s "マルチバイト文字及びマルチバイトURLを含む "
            , l "http://example.com" "example.com"
            , s " "
            , l "http://マルチバイト.jp" "マルチバイト.jp"
            ]
        ]


testAutoLinker : String -> List (Inline ()) -> Test
testAutoLinker initial expected =
    test ("should work for text: '" ++ initial ++ "'") <|
        \_ ->
            TextParser.parse { parseOptions | autoLink = True } initial
                |> equalParsed [ Paragraph "" expected ]


unescapeTagsSuite : Test
unescapeTagsSuite =
    describe "unescapeTags"
        [ testUnescapeTags "" [ Text "" ]
        , testUnescapeTags "a" [ Text "a" ]
        , testUnescapeTags "aa" [ Text "aa" ]
        , testUnescapeTags "a&" [ Text "a&" ]
        , testUnescapeTags "&a" [ Text "&a" ]
        , testUnescapeTags "&&" [ Text "&&" ]
        , testUnescapeTags "&gt;" [ Text ">" ]
        , testUnescapeTags "&lt;" [ Text "<" ]
        , testUnescapeTags "&amp;" [ Text "&amp;" ]
        , testUnescapeTags "a&gt;" [ Text "a>" ]
        , testUnescapeTags "&gt;a" [ Text ">a" ]
        , testUnescapeTags "a&gt;a" [ Text "a>a" ]
        , testUnescapeTags "&gta;" [ Text "&gta;" ]
        , testUnescapeTags "&agt;" [ Text "&agt;" ]
        , testUnescapeTags "&&gt;" [ Text "&>" ]
        , testUnescapeTags "&gt;&" [ Text ">&" ]
        , testUnescapeTags "&gt;&gt;" [ Text ">>" ]
        ]


testUnescapeTags : String -> List (Inline ()) -> Test
testUnescapeTags initial expected =
    test ("should work for text: '" ++ initial ++ "'") <|
        \_ ->
            TextParser.parse { parseOptions | unescapeTags = True } initial
                |> equalParsed [ Paragraph "" expected ]



-- Data.Filter


filterSuite : Test
filterSuite =
    let
        ofDiscordChannel channelId =
            OfDiscordChannel (Id.from channelId)

        ofSlackConversation convoId =
            OfSlackConversation (Id.from convoId)
    in
    describe "Data.Filter"
        [ describe "compareFilterAtom"
            [ testcompareFilterAtom (ofDiscordChannel "b") (ofDiscordChannel "a") GT
            , testcompareFilterAtom (ofDiscordChannel "a") (ofDiscordChannel "a") EQ
            , testcompareFilterAtom (ofDiscordChannel "a") (ofDiscordChannel "b") LT
            , testcompareFilterAtom (ofDiscordChannel "a") (ofSlackConversation "a") LT
            , testcompareFilterAtom (ofDiscordChannel "a") (ByMessage "a") LT
            , testcompareFilterAtom (ofDiscordChannel "a") (ByMedia HasImage) LT
            , testcompareFilterAtom (ofDiscordChannel "a") RemoveMe LT
            , testcompareFilterAtom (ofSlackConversation "a") (ofDiscordChannel "a") GT
            , testcompareFilterAtom (ofSlackConversation "b") (ofSlackConversation "a") GT
            , testcompareFilterAtom (ofSlackConversation "a") (ofSlackConversation "a") EQ
            , testcompareFilterAtom (ofSlackConversation "a") (ofSlackConversation "b") LT
            , testcompareFilterAtom (ofSlackConversation "a") (ByMessage "a") LT
            , testcompareFilterAtom (ofSlackConversation "a") (ByMedia HasImage) LT
            , testcompareFilterAtom (ofSlackConversation "a") RemoveMe LT
            , testcompareFilterAtom (ByMessage "a") (ofDiscordChannel "a") GT
            , testcompareFilterAtom (ByMessage "a") (ofSlackConversation "a") GT
            , testcompareFilterAtom (ByMessage "b") (ByMessage "a") GT
            , testcompareFilterAtom (ByMessage "a") (ByMessage "a") EQ
            , testcompareFilterAtom (ByMessage "a") (ByMessage "b") LT
            , testcompareFilterAtom (ByMessage "a") (ByMedia HasNone) LT
            , testcompareFilterAtom (ByMessage "a") RemoveMe LT
            , testcompareFilterAtom (ByMedia HasImage) (ofDiscordChannel "a") GT
            , testcompareFilterAtom (ByMedia HasImage) (ofSlackConversation "a") GT
            , testcompareFilterAtom (ByMedia HasImage) (ByMessage "a") GT
            , testcompareFilterAtom (ByMedia HasNone) (ByMedia HasImage) GT
            , testcompareFilterAtom (ByMedia HasNone) (ByMedia HasVideo) GT
            , testcompareFilterAtom (ByMedia HasVideo) (ByMedia HasImage) GT
            , testcompareFilterAtom (ByMedia HasImage) (ByMedia HasImage) EQ
            , testcompareFilterAtom (ByMedia HasVideo) (ByMedia HasVideo) EQ
            , testcompareFilterAtom (ByMedia HasNone) (ByMedia HasNone) EQ
            , testcompareFilterAtom (ByMedia HasImage) (ByMedia HasVideo) LT
            , testcompareFilterAtom (ByMedia HasImage) (ByMedia HasNone) LT
            , testcompareFilterAtom (ByMedia HasVideo) (ByMedia HasNone) LT
            , testcompareFilterAtom (ByMedia HasImage) RemoveMe LT
            , testcompareFilterAtom RemoveMe (ofDiscordChannel "a") GT
            , testcompareFilterAtom RemoveMe (ofSlackConversation "a") GT
            , testcompareFilterAtom RemoveMe (ByMessage "a") GT
            , testcompareFilterAtom RemoveMe (ByMedia HasImage) GT
            , testcompareFilterAtom RemoveMe RemoveMe EQ
            ]
        ]


testcompareFilterAtom : FilterAtom -> FilterAtom -> Order -> Test
testcompareFilterAtom fa1 fa2 expected =
    test (String.join " " [ Debug.toString fa1, Debug.toString expected, Debug.toString fa2 ]) <|
        \_ ->
            Filter.compareFilterAtom fa1 fa2 |> Expect.equal expected



-- Data.Producer.FetchStatus


fetchStatusSuite : Test
fetchStatusSuite =
    describe "Data.Producer.FetchStatus"
        [ describe "compare"
            [ testCompare Waiting Waiting EQ
            , testCompare Waiting (NextFetchAt (po 1) BO10) LT
            , testCompare Waiting (Fetching (po 1) BO10) LT
            , testCompare Waiting (InitialFetching (po 1)) LT
            , testCompare Waiting Available LT
            , testCompare (NextFetchAt (po 1) BO10) Waiting GT
            , testCompare (NextFetchAt (po 1) BO10) (NextFetchAt (po 0) BO10) GT
            , testCompare (NextFetchAt (po 1) BO10) (NextFetchAt (po 1) BO10) EQ
            , testCompare (NextFetchAt (po 1) BO10) (NextFetchAt (po 1) BO20) EQ
            , testCompare (NextFetchAt (po 1) BO10) (NextFetchAt (po 2) BO10) LT
            , testCompare (NextFetchAt (po 1) BO10) (Fetching (po 1) BO10) LT
            , testCompare (NextFetchAt (po 1) BO10) (InitialFetching (po 1)) LT
            , testCompare (NextFetchAt (po 1) BO10) Available LT
            , testCompare (Fetching (po 1) BO10) Waiting GT
            , testCompare (Fetching (po 1) BO10) (NextFetchAt (po 1) BO10) GT
            , testCompare (Fetching (po 1) BO10) (Fetching (po 0) BO10) GT
            , testCompare (Fetching (po 1) BO10) (Fetching (po 1) BO10) EQ
            , testCompare (Fetching (po 1) BO10) (Fetching (po 1) BO20) EQ
            , testCompare (Fetching (po 1) BO10) (Fetching (po 2) BO10) LT
            , testCompare (Fetching (po 1) BO10) (InitialFetching (po 1)) LT
            , testCompare (Fetching (po 1) BO10) Available LT
            , testCompare (InitialFetching (po 1)) Waiting GT
            , testCompare (InitialFetching (po 1)) (NextFetchAt (po 1) BO10) GT
            , testCompare (InitialFetching (po 1)) (Fetching (po 1) BO10) GT
            , testCompare (InitialFetching (po 1)) (InitialFetching (po 0)) GT
            , testCompare (InitialFetching (po 1)) (InitialFetching (po 1)) EQ
            , testCompare (InitialFetching (po 1)) (InitialFetching (po 2)) LT
            , testCompare (InitialFetching (po 1)) Available LT
            , testCompare Available Waiting GT
            , testCompare Available (NextFetchAt (po 1) BO10) GT
            , testCompare Available (Fetching (po 1) BO10) GT
            , testCompare Available (InitialFetching (po 1)) GT
            , testCompare Available Available EQ
            ]
        , describe "lessThan"
            [ Waiting |> testLessThan (NextFetchAt (po 1) BO10)
            , NextFetchAt (po 0) BO10 |> testLessThan (NextFetchAt (po 1) BO10)
            ]
        ]


testCompare : FetchStatus -> FetchStatus -> Order -> Test
testCompare a b order =
    test ("'" ++ Debug.toString a ++ "' " ++ Debug.toString order ++ " '" ++ Debug.toString b ++ "'") <|
        \_ ->
            FetchStatus.compare a b |> Expect.equal order


testLessThan : FetchStatus -> FetchStatus -> Test
testLessThan a b =
    test ("'" ++ Debug.toString b ++ "' is lessThan '" ++ Debug.toString a ++ "'") <|
        \_ ->
            FetchStatus.lessThan a b
                |> Expect.true ("'" ++ Debug.toString b ++ "' is NOT lessThan '" ++ Debug.toString a ++ "'")



-- Data.Producer.Discord


discordSuite : Test
discordSuite =
    describe "Data.Producer.Discord"
        [ describe "colorDecoder/encodeColor"
            [ testColorIntCodec "0" "000000"
            , testColorIntCodec "15" "00000f"
            , testColorIntCodec "255" "0000ff"
            , testColorIntCodec "4095" "000fff"
            , testColorIntCodec "65535" "00ffff"
            , testColorIntCodec "1048575" "0fffff"
            , testColorIntCodec "16777215" "ffffff"
            ]
        ]


testColorIntCodec : String -> String -> Test
testColorIntCodec colorNumStr expectedHex =
    test ("should decode/encode color integer " ++ colorNumStr) <|
        \_ ->
            colorNumStr
                |> D.decodeString Data.Producer.Discord.Message.colorDecoder
                |> Result.map (ColorExtra.encode >> E.encode 0)
                |> Result.andThen (D.decodeString Data.Producer.Discord.Message.colorDecoder)
                |> Expect.equal (Ok (ColorExtra.fromHexUnsafe expectedHex))



-- Data.Producer.Slack


slackSuite : Test
slackSuite =
    describe "Data.Producer.Slack"
        [ testCodec "should decode/encode User"
            SlackTestData.userInfoJson
            (D.field "user" SlackUser.decoder)
            SlackUser.encode
            SlackUser.decoder
        , testCodec "should decode/encode Team"
            SlackTestData.teamInfoJson
            (D.field "team" SlackTeam.decoder)
            SlackTeam.encode
            SlackTeam.decoder
        , testCodec "should decode/encode Conversation list"
            SlackTestData.convListJson
            (D.field "channels" (D.list (SlackConvo.decoderForApiResponse Dict.empty)))
            (E.list SlackConvo.encode)
            (D.list SlackConvo.decoder)
        , testCodec "should decode/encode Message list"
            SlackTestData.conversationHistoryJson
            (D.field "messages" (D.list (SlackMessage.decoderForApiResponse Dict.empty Dict.empty Dict.empty (Id.from "CDUMMYID"))))
            (E.list SlackMessage.encode)
            (D.list SlackMessage.decoder)
        , testCodec "should decode/encode Bot"
            SlackTestData.botInfoJson
            (D.field "bot" SlackBot.decoder)
            SlackBot.encode
            SlackBot.decoder
        , let
            c name type_ =
                SlackConvo.convo
                    (Id.from "CDUMMYID")
                    name
                    False
                    Nothing
                    type_
                    Available
          in
          describe "compare"
            [ testCompareConvo (c "Name" (PublicChannel True)) (c "Aaaa" (PublicChannel True)) GT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Name" (PublicChannel True)) EQ
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Zzzz" (PublicChannel True)) LT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Name" PrivateChannel) LT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "USER" IM) LT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Users" MPIM) LT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Aaaa" (PublicChannel False)) LT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Name" (PublicChannel False)) LT
            , testCompareConvo (c "Name" (PublicChannel True)) (c "Zzzz" (PublicChannel False)) LT
            , testCompareConvo (c "Name" PrivateChannel) (c "Name" (PublicChannel True)) GT
            , testCompareConvo (c "Name" PrivateChannel) (c "Aaaa" PrivateChannel) GT
            , testCompareConvo (c "Name" PrivateChannel) (c "Name" PrivateChannel) EQ
            , testCompareConvo (c "Name" PrivateChannel) (c "Zzzz" PrivateChannel) LT
            , testCompareConvo (c "Name" PrivateChannel) (c "USER" IM) LT
            , testCompareConvo (c "Name" PrivateChannel) (c "Users" MPIM) LT
            , testCompareConvo (c "Name" PrivateChannel) (c "Name" (PublicChannel False)) LT
            , testCompareConvo (c "USER" IM) (c "Name" (PublicChannel True)) GT
            , testCompareConvo (c "USER" IM) (c "Name" PrivateChannel) GT
            , testCompareConvo (c "USER" IM) (c "AAAA" IM) GT
            , testCompareConvo (c "USER" IM) (c "USER" IM) EQ
            , testCompareConvo (c "USER" IM) (c "ZZZZ" IM) LT
            , testCompareConvo (c "USER" IM) (c "Users" MPIM) LT
            , testCompareConvo (c "USER" IM) (c "Name" (PublicChannel False)) LT
            , testCompareConvo (c "Users" MPIM) (c "Name" (PublicChannel True)) GT
            , testCompareConvo (c "Users" MPIM) (c "Name" PrivateChannel) GT
            , testCompareConvo (c "Users" MPIM) (c "USER" IM) GT
            , testCompareConvo (c "Users" MPIM) (c "Aaaaa" MPIM) GT
            , testCompareConvo (c "Users" MPIM) (c "Users" MPIM) EQ
            , testCompareConvo (c "Users" MPIM) (c "Zzzzz" MPIM) LT
            , testCompareConvo (c "Users" MPIM) (c "Name" (PublicChannel False)) LT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Aaaa" (PublicChannel True)) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Name" (PublicChannel True)) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Zzzz" (PublicChannel True)) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Name" PrivateChannel) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "USER" IM) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Users" MPIM) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Aaaa" (PublicChannel False)) GT
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Name" (PublicChannel False)) EQ
            , testCompareConvo (c "Name" (PublicChannel False)) (c "Zzzz" (PublicChannel False)) LT
            ]
        , describe "resolveAngleCmd"
            [ testAngleCmd
                (String.join "\n"
                    [ "*<https://github.com/ymtsze/zephyr/commit/sha01234|1 new commit> pushed to <https://github.com/ymtsze/zephyr/tree/master|`master`>*"
                    , "<https://github.com/ymtsze/zephyr/commit/sha01234|`sha01234`> - Commit message here"
                    ]
                )
                (String.join "\n"
                    [ "*[1 new commit](https://github.com/ymtsze/zephyr/commit/sha01234) pushed to [`master`](https://github.com/ymtsze/zephyr/tree/master)*"
                    , "[`sha01234`](https://github.com/ymtsze/zephyr/commit/sha01234) - Commit message here"
                    ]
                )
            , testAngleCmd
                "*<https://github.com/ymtszw/zephyr/compare/a6d8e0918188...86e82fd07d55|8 new commits> pushed to <https://github.com/ymtszw/zephyr/tree/#10_parse_markdowns|`#10_parse_markdowns`>*\n"
                "*[8 new commits](https://github.com/ymtszw/zephyr/compare/a6d8e0918188...86e82fd07d55) pushed to [`#10_parse_markdowns`](https://github.com/ymtszw/zephyr/tree/#10_parse_markdowns)*\n"
            , testAngleCmd
                (String.join "\n"
                    [ "*<https://github.com/ymtszw/zephyr/compare/03298394604b...16bc78e06fba|10 new commits> pushed to <https://github.com/ymtszw/zephyr/tree/master|`master`>*"
                    , "<https://github.com/ymtszw/zephyr/commit/0b5178c7e80d8e7cc36041fd74f34eff7ce289d0|`0b5178c7`> - [#45] Slack Attachment texts"
                    , "<https://github.com/ymtszw/zephyr/commit/528060db6c041bd16f160a39e8ca4e86a0e81987|`528060db`> - [#45] Add suspicious data"
                    , "<https://github.com/ymtszw/zephyr/commit/2897572a41a9353ff311c0bd73aa6a40a4c66006|`2897572a`> - [#45] Fix: handle attachment without text"
                    , "<https://github.com/ymtszw/zephyr/commit/6c424d41e3087567fc5187c3f98d27d67522805e|`6c424d41`> - [#45] Introduce debug entry point in leakyList"
                    , "<https://github.com/ymtszw/zephyr/commit/d69ae4c785e1795a95b55e6bdc3a58b4f3bf6684|`d69ae4c7`> - [#45] Style: remove static icon background, only set if not URL is given"
                    , "<https://github.com/ymtszw/zephyr/commit/086b224beed236487722c4d6748e9a3017b75366|`086b224b`> - [#45] Rename messageToParagraph =&gt; collapsingParagraph since it"
                    , "<https://github.com/ymtszw/zephyr/commit/8ced322c0e026e8f8fb0863c2a105df57701300b|`8ced322c`> - [#45] Use fallback in attachment when other contents are unavailable"
                    ]
                )
                (String.join "\n"
                    [ "*[10 new commits](https://github.com/ymtszw/zephyr/compare/03298394604b...16bc78e06fba) pushed to [`master`](https://github.com/ymtszw/zephyr/tree/master)*"
                    , "[`0b5178c7`](https://github.com/ymtszw/zephyr/commit/0b5178c7e80d8e7cc36041fd74f34eff7ce289d0) - [#45] Slack Attachment texts"
                    , "[`528060db`](https://github.com/ymtszw/zephyr/commit/528060db6c041bd16f160a39e8ca4e86a0e81987) - [#45] Add suspicious data"
                    , "[`2897572a`](https://github.com/ymtszw/zephyr/commit/2897572a41a9353ff311c0bd73aa6a40a4c66006) - [#45] Fix: handle attachment without text"
                    , "[`6c424d41`](https://github.com/ymtszw/zephyr/commit/6c424d41e3087567fc5187c3f98d27d67522805e) - [#45] Introduce debug entry point in leakyList"
                    , "[`d69ae4c7`](https://github.com/ymtszw/zephyr/commit/d69ae4c785e1795a95b55e6bdc3a58b4f3bf6684) - [#45] Style: remove static icon background, only set if not URL is given"
                    , "[`086b224b`](https://github.com/ymtszw/zephyr/commit/086b224beed236487722c4d6748e9a3017b75366) - [#45] Rename messageToParagraph =&gt; collapsingParagraph since it"
                    , "[`8ced322c`](https://github.com/ymtszw/zephyr/commit/8ced322c0e026e8f8fb0863c2a105df57701300b) - [#45] Use fallback in attachment when other contents are unavailable"
                    ]
                )
            , testAngleCmd
                "<@USLACKBOT> Hi!\n<!here> <!channel> Yo!\n<#CDUMMYID> You go here. A.k.a <#CDUMMYID|hell>."
                "@USLACKBOT Hi!\n@here @channel Yo!\n#CDUMMYID You go here. A.k.a #hell."
            , testAngleCmd
                (String.join "\n"
                    [ "*<https://github.com/ymtszw/zephyr/compare/bb19a8040ed5...8941a3d4460d|68 new commits> pushed to <https://github.com/ymtszw/zephyr/tree/master|`master`>*"
                    , "<https://github.com/ymtszw/zephyr/commit/294b6dc729be05674d26acb0407195fc5d0c7461|`294b6dc7`> - [#59] Rename Sidebar.Effects members"
                    , "<https://github.com/ymtszw/zephyr/commit/0c9c42eb2db867afd773bef07d4eb112457842a5|`0c9c42eb`> - [#59] Use Atoms.Input.Select component APIs"

                    -- Many lines snipped from original data
                    , "<https://github.com/ymtszw/zephyr/commit/7dedb66c47472adf873560db7116acdd8c9b0045|`7dedb66c`> - [#59] Add/test pretext in EmbeddedMatter"
                    , "<https://github.com/ymtszw/zephyr/commit/0619141ff7604578a5979e06ceb8e47aaf2fdc2b|`…" -- Angle is not closed!
                    ]
                )
                (String.join "\n"
                    [ "*[68 new commits](https://github.com/ymtszw/zephyr/compare/bb19a8040ed5...8941a3d4460d) pushed to [`master`](https://github.com/ymtszw/zephyr/tree/master)*"
                    , "[`294b6dc7`](https://github.com/ymtszw/zephyr/commit/294b6dc729be05674d26acb0407195fc5d0c7461) - [#59] Rename Sidebar.Effects members"
                    , "[`0c9c42eb`](https://github.com/ymtszw/zephyr/commit/0c9c42eb2db867afd773bef07d4eb112457842a5) - [#59] Use Atoms.Input.Select component APIs"
                    , "[`7dedb66c`](https://github.com/ymtszw/zephyr/commit/7dedb66c47472adf873560db7116acdd8c9b0045) - [#59] Add/test pretext in EmbeddedMatter"
                    , "<https://github.com/ymtszw/zephyr/commit/0619141ff7604578a5979e06ceb8e47aaf2fdc2b|`…"
                    ]
                )
            ]
        ]


testCodec : String -> String -> Decoder a -> (a -> Value) -> Decoder a -> Test
testCodec desc initialData entryDecoder encodeForPersist savedStateDecoder =
    test desc <|
        \_ ->
            case D.decodeString entryDecoder initialData of
                Ok enteredData ->
                    enteredData
                        |> encodeForPersist
                        |> E.encode 0
                        |> D.decodeString savedStateDecoder
                        -- Must exactly match after state recovery
                        |> Expect.equal (Ok enteredData)

                Err e ->
                    Expect.fail <| "Failed to decode on entry: " ++ D.errorToString e


testCompareConvo : SlackConvo.Convo -> SlackConvo.Convo -> Order -> Test
testCompareConvo a b order =
    test ("'" ++ Debug.toString a ++ "' " ++ Debug.toString order ++ " '" ++ Debug.toString b ++ "'") <|
        \_ ->
            SlackConvo.compare a b |> Expect.equal order


testAngleCmd : String -> String -> Test
testAngleCmd initial expected =
    test ("should resolve angle cmds in: " ++ initial) <|
        \_ ->
            AngleCmd.resolve Dict.empty Dict.empty initial
                |> Expect.equal expected
