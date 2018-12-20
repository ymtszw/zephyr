module Examples exposing (suite)

import Array exposing (fromList)
import ArrayExtra as Array
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Data.Producer.Slack as Slack exposing (ConversationType(..))
import Data.TextRenderer exposing (StringOrUrl(..))
import Data.UniqueIdGen exposing (UniqueIdGen)
import Dict
import Element exposing (rgb255)
import Expect exposing (Expectation)
import Fuzz
import Hex
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Json.EncodeExtra as E
import ListExtra
import Markdown.Block exposing (Block(..), CodeBlock(..))
import Markdown.Inline exposing (Inline(..))
import Parser
import SelectArray
import SlackTestData
import String exposing (fromInt, toInt)
import StringExtra
import Test exposing (..)
import TextParser exposing (Parsed(..))
import Time exposing (Posix)
import TimeExtra exposing (po)


suite : Test
suite =
    describe "test"
        [ selectArraySuite
        , listSuite
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



-- ListExtra


listSuite : Test
listSuite =
    describe "ListExtra"
        [ describe "groupWhile"
            [ testGroupWhile (==) [] []
            , testGroupWhile (==) [ 0 ] [ [ 0 ] ]
            , testGroupWhile (==) [ 0, 1, 2 ] [ [ 0 ], [ 1 ], [ 2 ] ]
            , testGroupWhile (==) [ 0, 0, 2 ] [ [ 0, 0 ], [ 2 ] ]
            , testGroupWhile (==) [ 0, 1, 1 ] [ [ 0 ], [ 1, 1 ] ]
            , testGroupWhile (==) [ 0, 1, 0 ] [ [ 0 ], [ 1 ], [ 0 ] ]
            , testGroupWhile (<) [ 0, 1, 2 ] [ [ 0, 1, 2 ] ]
            , testGroupWhile (<) [ 0, 1, 0 ] [ [ 0, 1 ], [ 0 ] ]
            , testGroupWhile (<) [ 2, 1, 0 ] [ [ 2 ], [ 1 ], [ 0 ] ]
            , testGroupWhile (<) [ 2, 1, 2 ] [ [ 2 ], [ 1, 2 ] ]
            ]
        ]


testGroupWhile : (Int -> Int -> Bool) -> List Int -> List (List Int) -> Test
testGroupWhile checker initial expected =
    test ("should group " ++ Debug.toString initial ++ " to " ++ Debug.toString expected) <|
        \_ ->
            initial
                |> ListExtra.groupWhile checker
                |> Expect.all
                    [ Expect.equal expected
                    , List.concat >> Expect.equal initial
                    ]



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
        , slackParseSuite
        , autoLinkerSuite
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
                [ Text "After blank line.\nSoft line break and "
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
                |> Expect.equal (Parsed expected)


slackParseSuite : Test
slackParseSuite =
    describe "parse with Slack options"
        [ testSlackParse "*<https://github.com/ymtsze/zephyr/commit/sha01234|1 new commit> pushed to <https://github.com/ymtsze/zephyr/tree/master|`master`>*\n<https://github.com/ymtsze/zephyr/commit/sha01234|`sha01234`> - Commit message here"
            [ Paragraph ""
                [ Emphasis 1
                    [ Link "https://github.com/ymtsze/zephyr/commit/sha01234" Nothing [ Text "1 new commit" ]
                    , Text " pushed to "
                    , Link "https://github.com/ymtsze/zephyr/tree/master" Nothing [ CodeInline "master" ]
                    ]
                , Text "\n"
                , Link "https://github.com/ymtsze/zephyr/commit/sha01234" Nothing [ CodeInline "sha01234" ]
                , Text " - Commit message here"
                ]
            ]
        , testSlackParse """*<https://github.com/ymtszw/zephyr/compare/03298394604b...16bc78e06fba|10 new commits> pushed to <https://github.com/ymtszw/zephyr/tree/master|`master`>*
<https://github.com/ymtszw/zephyr/commit/0b5178c7e80d8e7cc36041fd74f34eff7ce289d0|`0b5178c7`> - [#45] Slack Attachment texts
<https://github.com/ymtszw/zephyr/commit/528060db6c041bd16f160a39e8ca4e86a0e81987|`528060db`> - [#45] Add suspicious data
<https://github.com/ymtszw/zephyr/commit/2897572a41a9353ff311c0bd73aa6a40a4c66006|`2897572a`> - [#45] Fix: handle attachment without text
<https://github.com/ymtszw/zephyr/commit/6c424d41e3087567fc5187c3f98d27d67522805e|`6c424d41`> - [#45] Introduce debug entry point in leakyList
<https://github.com/ymtszw/zephyr/commit/d69ae4c785e1795a95b55e6bdc3a58b4f3bf6684|`d69ae4c7`> - [#45] Style: remove static icon background, only set if not URL is given
<https://github.com/ymtszw/zephyr/commit/086b224beed236487722c4d6748e9a3017b75366|`086b224b`> - [#45] Rename messageToParagraph =&gt; collapsingParagraph since it
<https://github.com/ymtszw/zephyr/commit/8ced322c0e026e8f8fb0863c2a105df57701300b|`8ced322c`> - [#45] Use fallback in attachment when other contents are unavailable"""
            [ Paragraph ""
                [ Emphasis 1
                    [ Link "https://github.com/ymtszw/zephyr/compare/03298394604b...16bc78e06fba" Nothing [ Text "10 new commits" ]
                    , Text " pushed to "
                    , Link "https://github.com/ymtszw/zephyr/tree/master" Nothing [ CodeInline "master" ]
                    ]
                , Text "\n"
                , Link "https://github.com/ymtszw/zephyr/commit/0b5178c7e80d8e7cc36041fd74f34eff7ce289d0" Nothing [ CodeInline "0b5178c7" ]
                , Text " - [#45] Slack Attachment texts\n"
                , Link "https://github.com/ymtszw/zephyr/commit/528060db6c041bd16f160a39e8ca4e86a0e81987" Nothing [ CodeInline "528060db" ]
                , Text " - [#45] Add suspicious data\n"
                , Link "https://github.com/ymtszw/zephyr/commit/2897572a41a9353ff311c0bd73aa6a40a4c66006" Nothing [ CodeInline "2897572a" ]
                , Text " - [#45] Fix: handle attachment without text\n"
                , Link "https://github.com/ymtszw/zephyr/commit/6c424d41e3087567fc5187c3f98d27d67522805e" Nothing [ CodeInline "6c424d41" ]
                , Text " - [#45] Introduce debug entry point in leakyList\n"
                , Link "https://github.com/ymtszw/zephyr/commit/d69ae4c785e1795a95b55e6bdc3a58b4f3bf6684" Nothing [ CodeInline "d69ae4c7" ]
                , Text " - [#45] Style: remove static icon background, only set if not URL is given\n"
                , Link "https://github.com/ymtszw/zephyr/commit/086b224beed236487722c4d6748e9a3017b75366" Nothing [ CodeInline "086b224b" ]
                , Text " - [#45] Rename messageToParagraph => collapsingParagraph since it\n"
                , Link "https://github.com/ymtszw/zephyr/commit/8ced322c0e026e8f8fb0863c2a105df57701300b" Nothing [ CodeInline "8ced322c" ]
                , Text " - [#45] Use fallback in attachment when other contents are unavailable"
                ]
            ]
        , testSlackParse "&lt;pre&gt;This is code block&lt;/pre&gt;&lt;p&gt;&lt;code&gt;this is inline code&lt;/code&gt;&lt;/p&gt;"
            [ Paragraph ""
                [ HtmlInline "pre" [] [ Text "This is code block" ]
                , HtmlInline "p" [] [ HtmlInline "code" [] [ Text "this is inline code" ] ]
                ]
            ]
        ]


testSlackParse : String -> List (Block () ()) -> Test
testSlackParse initial expected =
    test ("should parse: " ++ initial ++ "") <|
        \_ ->
            TextParser.parse (Slack.parseOptions Dict.empty Dict.empty) initial
                |> Expect.equal (Parsed expected)


autoLinkerSuite : Test
autoLinkerSuite =
    let
        s =
            Text

        l urlStr title =
            Link urlStr Nothing [ Text title ]
    in
    describe "autoLinker"
        [ testAutoLinker "" []
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
            let
                onlyAutoLink =
                    { markdown = False
                    , autoLink = True
                    , unescapeTags = False
                    , preFormat = Nothing
                    , customInlineFormat = Nothing
                    }
            in
            TextParser.parse onlyAutoLink initial
                |> Expect.equal (Parsed [ Paragraph "" expected ])



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
            (D.field "messages" (D.list (Slack.apiMessageDecoder Dict.empty Dict.empty "CDUMMYID")))
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
