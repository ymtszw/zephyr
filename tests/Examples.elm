module Examples exposing (suite)

import Array exposing (fromList)
import ArrayExtra as Array
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Data.Producer.Slack as Slack exposing (ConversationType(..))
import Data.UniqueIdGen exposing (UniqueIdGen)
import Dict
import Element exposing (rgb255)
import Expect exposing (Expectation)
import Fuzz
import Hex
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
        , uniqueIdSuite
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



-- Data.UniqueIdGen


uniqueIdSuite : Test
uniqueIdSuite =
    describe "Data.UniqueIdGen"
        [ describe "gen"
            [ testGen [ ( ( "prefixA", 0 ), "not generated" ) ]
            , testGen [ ( ( "prefixA", 1 ), "prefixA_0" ) ]
            , testGen [ ( ( "prefixA", 11 ), "prefixA_10" ) ]
            , testGen [ ( ( "prefixA", 101 ), "prefixA_100" ) ]
            , testGen
                [ ( ( "prefixA", 10 ), "prefixA_9" )
                , ( ( "prefixB", 20 ), "prefixB_19" )
                , ( ( "prefixC", 0 ), "not generated" )
                ]
            ]
        ]


testGen : List ( ( String, Int ), String ) -> Test
testGen reqExpects =
    let
        ( requests, expects ) =
            List.unzip reqExpects
    in
    test ("should works for requests: " ++ Debug.toString requests) <|
        \_ ->
            let
                ( actuals, _ ) =
                    genAll requests Data.UniqueIdGen.init
            in
            Expect.equalLists expects actuals


genAll : List ( String, Int ) -> UniqueIdGen -> ( List String, UniqueIdGen )
genAll requests generator =
    let
        folder ( prefix, howMany ) ( accGenerated, accGenerator ) =
            let
                ( generated, newGenerator ) =
                    seqGen prefix howMany accGenerator
            in
            ( generated :: accGenerated, newGenerator )
    in
    List.foldr folder ( [], generator ) requests


seqGen : String -> Int -> UniqueIdGen -> ( String, UniqueIdGen )
seqGen prefix howMany generator =
    seqGenImpl prefix howMany ( "not generated", generator )


seqGenImpl : String -> Int -> ( String, UniqueIdGen ) -> ( String, UniqueIdGen )
seqGenImpl prefix howMany ( lastResult, accGenerator ) =
    if howMany <= 0 then
        ( lastResult, accGenerator )

    else
        seqGenImpl prefix (howMany - 1) (Data.UniqueIdGen.gen prefix accGenerator)



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
            TextParser.parse Slack.parseOptions initial
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
    describe "Data.Filter"
        [ describe "compareFilterAtom"
            [ testcompareFilterAtom (OfDiscordChannel "b") (OfDiscordChannel "a") GT
            , testcompareFilterAtom (OfDiscordChannel "a") (OfDiscordChannel "a") EQ
            , testcompareFilterAtom (OfDiscordChannel "a") (OfDiscordChannel "b") LT
            , testcompareFilterAtom (OfDiscordChannel "a") (OfSlackConversation "a") LT
            , testcompareFilterAtom (OfDiscordChannel "a") (ByMessage "a") LT
            , testcompareFilterAtom (OfDiscordChannel "a") (ByMedia HasImage) LT
            , testcompareFilterAtom (OfDiscordChannel "a") RemoveMe LT
            , testcompareFilterAtom (OfSlackConversation "a") (OfDiscordChannel "a") GT
            , testcompareFilterAtom (OfSlackConversation "b") (OfSlackConversation "a") GT
            , testcompareFilterAtom (OfSlackConversation "a") (OfSlackConversation "a") EQ
            , testcompareFilterAtom (OfSlackConversation "a") (OfSlackConversation "b") LT
            , testcompareFilterAtom (OfSlackConversation "a") (ByMessage "a") LT
            , testcompareFilterAtom (OfSlackConversation "a") (ByMedia HasImage) LT
            , testcompareFilterAtom (OfSlackConversation "a") RemoveMe LT
            , testcompareFilterAtom (ByMessage "a") (OfDiscordChannel "a") GT
            , testcompareFilterAtom (ByMessage "a") (OfSlackConversation "a") GT
            , testcompareFilterAtom (ByMessage "b") (ByMessage "a") GT
            , testcompareFilterAtom (ByMessage "a") (ByMessage "a") EQ
            , testcompareFilterAtom (ByMessage "a") (ByMessage "b") LT
            , testcompareFilterAtom (ByMessage "a") (ByMedia HasNone) LT
            , testcompareFilterAtom (ByMessage "a") RemoveMe LT
            , testcompareFilterAtom (ByMedia HasImage) (OfDiscordChannel "a") GT
            , testcompareFilterAtom (ByMedia HasImage) (OfSlackConversation "a") GT
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
            , testcompareFilterAtom RemoveMe (OfDiscordChannel "a") GT
            , testcompareFilterAtom RemoveMe (OfSlackConversation "a") GT
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
            let
                expectedColor =
                    Result.map3 rgb255
                        (expectedHex |> String.slice 0 2 |> Hex.fromString)
                        (expectedHex |> String.slice 2 4 |> Hex.fromString)
                        (expectedHex |> String.slice 4 6 |> Hex.fromString)
                        |> Result.withDefault (rgb255 0 0 0)
            in
            colorNumStr
                |> D.decodeString Data.Producer.Discord.colorDecoder
                |> Result.map (E.color >> E.encode 0)
                |> Result.andThen (D.decodeString Data.Producer.Discord.colorDecoder)
                |> Expect.equal (Ok expectedColor)



-- Data.Producer.Slack


slackSuite : Test
slackSuite =
    describe "Data.Producer.Slack"
        [ testCodec "should decode/encode User"
            SlackTestData.userInfoJson
            (D.field "user" Slack.userDecoder)
            Slack.encodeUser
            Slack.userDecoder
        , testCodec "should decode/encode Team"
            SlackTestData.teamInfoJson
            (D.field "team" Slack.teamDecoder)
            Slack.encodeTeam
            Slack.teamDecoder
        , testCodec "should decode/encode Conversation list"
            SlackTestData.convListJson
            (D.field "channels" (D.list (Slack.apiConversationDecoder Dict.empty)))
            (E.list Slack.encodeConversation)
            (D.list (Slack.conversationDecoder Dict.empty))
        , testCodec "should decode/encode Message list"
            SlackTestData.conversationHistoryJson
            (D.field "messages" (D.list (Slack.apiMessageDecoder Dict.empty Dict.empty Dict.empty "CDUMMYID")))
            (E.list Slack.encodeMessage)
            (D.list Slack.messageDecoder)
        , testCodec "should decode/encode Bot"
            SlackTestData.botInfoJson
            (D.field "bot" Slack.botDecoder)
            Slack.encodeBot
            Slack.botDecoder
        , let
            c name type_ =
                { id = Slack.dummyConversationId
                , name = name
                , isArchived = False
                , lastRead = Nothing
                , type_ = type_
                , fetchStatus = Available
                }
          in
          describe "compareByMembersipThenName"
            [ testCompareConversation (c "Name" (PublicChannel True)) (c "Aaaa" (PublicChannel True)) GT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Name" (PublicChannel True)) EQ
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Zzzz" (PublicChannel True)) LT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Name" PrivateChannel) LT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "USER" IM) LT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Users" MPIM) LT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Aaaa" (PublicChannel False)) LT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Name" (PublicChannel False)) LT
            , testCompareConversation (c "Name" (PublicChannel True)) (c "Zzzz" (PublicChannel False)) LT
            , testCompareConversation (c "Name" PrivateChannel) (c "Name" (PublicChannel True)) GT
            , testCompareConversation (c "Name" PrivateChannel) (c "Aaaa" PrivateChannel) GT
            , testCompareConversation (c "Name" PrivateChannel) (c "Name" PrivateChannel) EQ
            , testCompareConversation (c "Name" PrivateChannel) (c "Zzzz" PrivateChannel) LT
            , testCompareConversation (c "Name" PrivateChannel) (c "USER" IM) LT
            , testCompareConversation (c "Name" PrivateChannel) (c "Users" MPIM) LT
            , testCompareConversation (c "Name" PrivateChannel) (c "Name" (PublicChannel False)) LT
            , testCompareConversation (c "USER" IM) (c "Name" (PublicChannel True)) GT
            , testCompareConversation (c "USER" IM) (c "Name" PrivateChannel) GT
            , testCompareConversation (c "USER" IM) (c "AAAA" IM) GT
            , testCompareConversation (c "USER" IM) (c "USER" IM) EQ
            , testCompareConversation (c "USER" IM) (c "ZZZZ" IM) LT
            , testCompareConversation (c "USER" IM) (c "Users" MPIM) LT
            , testCompareConversation (c "USER" IM) (c "Name" (PublicChannel False)) LT
            , testCompareConversation (c "Users" MPIM) (c "Name" (PublicChannel True)) GT
            , testCompareConversation (c "Users" MPIM) (c "Name" PrivateChannel) GT
            , testCompareConversation (c "Users" MPIM) (c "USER" IM) GT
            , testCompareConversation (c "Users" MPIM) (c "Aaaaa" MPIM) GT
            , testCompareConversation (c "Users" MPIM) (c "Users" MPIM) EQ
            , testCompareConversation (c "Users" MPIM) (c "Zzzzz" MPIM) LT
            , testCompareConversation (c "Users" MPIM) (c "Name" (PublicChannel False)) LT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Aaaa" (PublicChannel True)) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Name" (PublicChannel True)) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Zzzz" (PublicChannel True)) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Name" PrivateChannel) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "USER" IM) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Users" MPIM) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Aaaa" (PublicChannel False)) GT
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Name" (PublicChannel False)) EQ
            , testCompareConversation (c "Name" (PublicChannel False)) (c "Zzzz" (PublicChannel False)) LT
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
                    , "<https://github.com/ymtszw/zephyr/commit/9badd2fffb80f094e3d274bcdc98a0ad07048f85|`9badd2ff`> - [#59] Expose constants from Templates.Main for scroll handling"
                    , "<https://github.com/ymtszw/zephyr/commit/7a9de2ac6a355946c5de86b278a1489cde207c35|`7a9de2ac`> - [#59] Stub Pages.Main"
                    , "<https://github.com/ymtszw/zephyr/commit/e1bf4ceddc96e19e6fde79cd68ffd9f316fa1a9b|`e1bf4ced`> - [#59] Wire real Model!!"
                    , "<https://github.com/ymtszw/zephyr/commit/6351035f6f3e6322f4044d4cc2dd6f7333e21b0c|`6351035f`> - [#59] Move msg-dependent member to Effects"
                    , "<https://github.com/ymtszw/zephyr/commit/dd3cada6f662e4baa29b093df98c6cde7ab82603|`dd3cada6`> - [#59] Expose FetchStatus.fetching"
                    , "<https://github.com/ymtszw/zephyr/commit/3222ff79809794cb6162c4ec68c110a5e1bd3fbf|`3222ff79`> - [#59] Clip texts in Icon"
                    , "<https://github.com/ymtszw/zephyr/commit/72f8a729398d413199c439c496f55ac88db6cf10|`72f8a729`> - [#59] Expose hydratedOnce, Subbable/SubbedChannel, tweak guild icon view"
                    , "<https://github.com/ymtszw/zephyr/commit/070128f039c73a8788f240cf70a0e93b6e7dac0b|`070128f0`> - [#59] Always block render, since otherwise \"collapsing\" of child"
                    , "<https://github.com/ymtszw/zephyr/commit/60f4cafbaefb2d957ebd05ebcff4f8731f7417b4|`60f4cafb`> - [#59] Update import sample"
                    , "<https://github.com/ymtszw/zephyr/commit/b5244142fcce0d076f87eda33a8f0289a2301211|`b5244142`> - [#59] Wire Producer.Discord model into Pages with data marshalling"
                    , "<https://github.com/ymtszw/zephyr/commit/73f2e9893928df26daff6c577efa839643a48c0c|`73f2e989`> - [#59] Expose Organisms.Config.Slack.hydratedOnce, move selectMsgTagger"
                    , "<https://github.com/ymtszw/zephyr/commit/f49f6c6c68a84f21c054687656b3624866ef4c46|`f49f6c6c`> - [#59] Take team.id as parameter of onRehydrateButtonClick"
                    , "<https://github.com/ymtszw/zephyr/commit/702e99a58ebc6d0fe6806aefe4419fbabf8294cf|`702e99a5`> - [#59] Take team.id in other effects too"
                    , "<https://github.com/ymtszw/zephyr/commit/2652aeac23789617098a46a2ff184a5d7a67ea68|`2652aeac`> - [#59] Expose Slack.isPrivate, fix team.image48 =&gt; image44"
                    , "<https://github.com/ymtszw/zephyr/commit/fef6e7681e25029c6f8866d973d5be018d680322|`fef6e768`> - [#59] Expose TeamSnip too, and add padding"
                    , "<https://github.com/ymtszw/zephyr/commit/4f618a90bf5e0f85c6f1825f790e1a50f2ba7c66|`4f618a90`> - [#59] Wire Select.sub"
                    , "<https://github.com/ymtszw/zephyr/commit/9fd6a71283bc783a0a47c122fdd6a24925fb5d99|`9fd6a712`> - [#59] Weed out positioning bug; `top` property does not need to be set"
                    , "<https://github.com/ymtszw/zephyr/commit/3a87c61fb5eeafeb916e170f06edc186e8be5b5c|`3a87c61f`> - [#59] Wire renderConfigSlack, refactor renderConfigDiscord"
                    , "<https://github.com/ymtszw/zephyr/commit/7b0f699ec0c98fccf1914eac2dd910acb74e9d02|`7b0f699e`> - [#59] Expose helper APIs, add some docs"
                    , "<https://github.com/ymtszw/zephyr/commit/320d31e1cbd7194d1aa0ddfa2c5dc81ba29b342d|`320d31e1`> - [#59] Wire Columns with marshalling to visible/shadow Columns"
                    , "<https://github.com/ymtszw/zephyr/commit/d83b615e1a2fef2364d2f00afae8776a21786760|`d83b615e`> - [#59] Rename effects and slightly change signatures"
                    , "<https://github.com/ymtszw/zephyr/commit/d2a377a09b0876d0c8bda6fbcfd426ff94ac479f|`d2a377a0`> - [#59] Apply renames"
                    , "<https://github.com/ymtszw/zephyr/commit/0945d361e2619f6083cf44526f318b38befe560a|`0945d361`> - [#59] Plug Header.render. Now column D&amp;D works"
                    , "<https://github.com/ymtszw/zephyr/commit/e003560acbbc4fb1fe31ee5959bdee731f60bae4|`e003560a`> - [#59] Take columnId in Column.Config.Effects.onCloseButtonClick"
                    , "<https://github.com/ymtszw/zephyr/commit/da6541c812b3ad211925a4358f3ae4a9bcb9215d|`da6541c8`> - [#59] Insert flexBasisAuto where required; show warings at"
                    , "<https://github.com/ymtszw/zephyr/commit/5be8e51ea257f9273ef78052d17c94bb7c8891ba|`5be8e51e`> - [#59] Wire Column.Config, although sourceSelector is not working."
                    , "<https://github.com/ymtszw/zephyr/commit/c9af709d71af2bb26731b03417c905569eb6f873|`c9af709d`> - [#59] Expose NamedEntity.desiredIconSize, align avatars to start"
                    , "<https://github.com/ymtszw/zephyr/commit/c663b63becae1f423ead5f61413b285fbfdb3d85|`c663b63b`> - [#59] Tweak font sizes"
                    , "<https://github.com/ymtszw/zephyr/commit/a15054d90d2cff3ce48b9810f3aa1e5420493d83|`a15054d9`> - [#59] Wire column.items into Column.Items, but WIP"
                    , "<https://github.com/ymtszw/zephyr/commit/e23f4a1ef437d4d1de75e39c784cc034bac80622|`e23f4a1e`> - [#59] Refactor: resolve column.id outside of Column.Items, fix typo"
                    , "<https://github.com/ymtszw/zephyr/commit/207125c6da124e99b542d051e503b53df7946f5b|`207125c6`> - [#59] Update sample codes in PLab"
                    , "<https://github.com/ymtszw/zephyr/commit/1d992aed1d82610cb2e5c4d6048887901ed62f0c|`1d992aed`> - [#59] Update Column.Items effects"
                    , "<https://github.com/ymtszw/zephyr/commit/4577f9cb399fffeb69cd81a26578ccdaa49f4800|`4577f9cb`> - [#59] Move scroll handler to templates rather than items"
                    , "<https://github.com/ymtszw/zephyr/commit/db2c216b62c32af2a9df6eac0ff092a704b652b4|`db2c216b`> - [#59] Log on NoOp in PatternLab"
                    , "<https://github.com/ymtszw/zephyr/commit/cf9b3db45ea6bc06d4edf91b43083263ece65755|`cf9b3db4`> - [#59] Move header clicker to container, rather than text part"
                    , "<https://github.com/ymtszw/zephyr/commit/6c9efa12ca50c2074f93f63afd15b0bb2a1ac74d|`6c9efa12`> - [#59] Apply scrollAttrs update in Pages.Main"
                    , "<https://github.com/ymtszw/zephyr/commit/48aacc8de46301a2f9c0f471aaed523e3911852f|`48aacc8d`> - [#59] Add borderFlash animation"
                    , "<https://github.com/ymtszw/zephyr/commit/7caa29dd3bffcd585d9e452271f9f172be2313aa|`7caa29dd`> - [#59] Take recentlyTouched and flash borders of column"
                    , "<https://github.com/ymtszw/zephyr/commit/69e70421d3f0efafd4ea984d2840aaa38c87c6e9|`69e70421`> - [#59] Test borderFlash in PatternLab"
                    , "<https://github.com/ymtszw/zephyr/commit/95fb12691e068ae70421d3b21eea3cc858799a6b|`95fb1269`> - [#59] Fix: use columnId rather than index for key"
                    , "<https://github.com/ymtszw/zephyr/commit/89fbf35db63f8e85bd42d8d048270b76ba95b5e8|`89fbf35d`> - [#59] Update Pages.Main"
                    , "<https://github.com/ymtszw/zephyr/commit/e5952dd680f9f1918da6ec515874ab2f3d0439cd|`e5952dd6`> - [#59] Use emebed.color"
                    , "<https://github.com/ymtszw/zephyr/commit/1dbb473e989ec3a26e3fa745d8d031b5d17743a3|`1dbb473e`> - [#59] Adjust itemGroupHeader"
                    , "<https://github.com/ymtszw/zephyr/commit/464822a03f91213ce6095a5588ff8c92ce7c9c85|`464822a0`> - [#59] Slightly change default markdown parse option, add note for"
                    , "<https://github.com/ymtszw/zephyr/commit/56c41523666d919bfd1ca2d3af856f3b14b730bb|`56c41523`> - [#59] Use records instead of tuples"
                    , "<https://github.com/ymtszw/zephyr/commit/6e827c1e9bea6547a03bc93a8c782688f4c73310|`6e827c1e`> - [#59] Fix: shadowing of decoders"
                    , "<https://github.com/ymtszw/zephyr/commit/d4908ec3ad12ba034c83f5548008b2765a901264|`d4908ec3`> - [#59] Only show Embed author avatar when they exist"
                    , "<https://github.com/ymtszw/zephyr/commit/900ba75f80a025205802d08aed963e11b636c613|`900ba75f`> - [#59] Slighly downsizing thumbnails"
                    , "<https://github.com/ymtszw/zephyr/commit/a1cfd26d2b0709913d6c2e08413d99f3bfcc132c|`a1cfd26d`> - [#59] Refactor: apOrId signature"
                    , "<https://github.com/ymtszw/zephyr/commit/a2428be52717637dab5425c3fae8e0a4e859e8ee|`a2428be5`> - [#59] Wire more contents in ColumnItem"
                    , "<https://github.com/ymtszw/zephyr/commit/3bdc972b7b378018de18e07387fc05ac7d4825cf|`3bdc972b`> - [#59] Fix test"
                    , "<https://github.com/ymtszw/zephyr/commit/49fb389acb6d37583a18fc134aa1e39f98aff75d|`49fb389a`> - [#59] Wire Slack attachments' thumbUrl/imageUrl"
                    , "<https://github.com/ymtszw/zephyr/commit/7dedb66c47472adf873560db7116acdd8c9b0045|`7dedb66c`> - [#59] Add/test pretext in EmbeddedMatter"
                    , "<https://github.com/ymtszw/zephyr/commit/0619141ff7604578a5979e06ceb8e47aaf2fdc2b|`…"
                    ]
                )
                ""
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


testCompareConversation : Slack.Conversation -> Slack.Conversation -> Order -> Test
testCompareConversation a b order =
    test ("'" ++ Debug.toString a ++ "' " ++ Debug.toString order ++ " '" ++ Debug.toString b ++ "'") <|
        \_ ->
            Slack.compareByMembersipThenName a b |> Expect.equal order


testAngleCmd : String -> String -> Test
testAngleCmd initial expected =
    test ("should resolve angle cmds in: " ++ initial) <|
        \_ ->
            Slack.resolveAngleCmd Dict.empty Dict.empty initial
                |> Expect.equal expected
