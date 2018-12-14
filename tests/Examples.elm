module Examples exposing (suite)

import Array exposing (fromList)
import ArrayExtra as Array
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Data.Producer.Slack as Slack
import Data.TextRenderer exposing (StringOrUrl(..))
import Data.UniqueIdGen exposing (UniqueIdGen)
import Element exposing (rgb255)
import Expect exposing (Expectation)
import Fuzz
import Hex
import Json.Decode as D exposing (Decoder, decodeString, decodeValue)
import Json.Encode exposing (Value, encode)
import Json.EncodeExtra
import ListExtra
import Parser
import SelectArray
import SlackTestData
import String exposing (fromInt, toInt)
import StringExtra
import Test exposing (..)
import Time exposing (Posix)
import TimeExtra exposing (po)
import Url


suite : Test
suite =
    describe "test"
        [ selectArraySuite
        , listSuite
        , stringSuite
        , arraySuite
        , uniqueIdSuite
        , textRendererSuite
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



-- Data.TextRenderer


textRendererSuite : Test
textRendererSuite =
    describe "Data.TextRenderer"
        [ describe "parseIntoStringOrUrlList"
            [ testParseIntoStringOrUrlList "" []
            , testParseIntoStringOrUrlList " " [ S " " ]
            , testParseIntoStringOrUrlList "foobar" [ S "foobar" ]
            , testParseIntoStringOrUrlList " very long text with\t\n spaces in it " [ S " very long text with\t\n spaces in it " ]
            , testParseIntoStringOrUrlList "text with unparsable http" [ S "text with unparsable http" ]
            , testParseIntoStringOrUrlList "text with unparsable http http" [ S "text with unparsable http http" ]
            , testParseIntoStringOrUrlList "http://example.com" [ U exampleCom ]
            , testParseIntoStringOrUrlList "http://example.com http://example.com" [ U exampleCom, S " ", U exampleCom ]
            , testParseIntoStringOrUrlList "text with parsable http://example.com" [ S "text with parsable ", U exampleCom ]
            , testParseIntoStringOrUrlList "textdirectlyfollowedbyhttp://example.com" [ S "textdirectlyfollowedby", U exampleCom ]
            , testParseIntoStringOrUrlList "textdirectlyfollowedbyhttp://example.comandnotdelimittedproperly"
                [ S "textdirectlyfollowedby"
                , U { exampleCom | host = "example.comandnotdelimittedproperly" }
                ]
            , testParseIntoStringOrUrlList "text with parsable http://example.com !" [ S "text with parsable ", U exampleCom, S " !" ]
            , testParseIntoStringOrUrlList "text with parsable http://example.com http://example.com !" [ S "text with parsable ", U exampleCom, S " ", U exampleCom, S " !" ]
            , testParseIntoStringOrUrlList "text with parsable http://example.com and unparsable http !" [ S "text with parsable ", U exampleCom, S " and unparsable http !" ]
            , testParseIntoStringOrUrlList "マルチバイト文字及びマルチバイトURLを含む http://example.com http://マルチバイト.jp"
                [ S "マルチバイト文字及びマルチバイトURLを含む "
                , U exampleCom
                , S " "
                , U { exampleCom | host = "マルチバイト.jp" }
                ]
            ]
        ]


testParseIntoStringOrUrlList : String -> List StringOrUrl -> Test
testParseIntoStringOrUrlList string expect =
    test ("should work for text: '" ++ string ++ "'") <|
        \_ ->
            string
                |> Parser.run Data.TextRenderer.parseIntoStringOrUrlList
                |> Expect.equal (Ok expect)


exampleCom : Url.Url
exampleCom =
    { protocol = Url.Http
    , host = "example.com"
    , port_ = Nothing
    , path = "/"
    , fragment = Nothing
    , query = Nothing
    }



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
            , testcompareFilterAtom (ByMedia HasNone) (ByMedia HasMovie) GT
            , testcompareFilterAtom (ByMedia HasMovie) (ByMedia HasImage) GT
            , testcompareFilterAtom (ByMedia HasImage) (ByMedia HasImage) EQ
            , testcompareFilterAtom (ByMedia HasMovie) (ByMedia HasMovie) EQ
            , testcompareFilterAtom (ByMedia HasNone) (ByMedia HasNone) EQ
            , testcompareFilterAtom (ByMedia HasImage) (ByMedia HasMovie) LT
            , testcompareFilterAtom (ByMedia HasImage) (ByMedia HasNone) LT
            , testcompareFilterAtom (ByMedia HasMovie) (ByMedia HasNone) LT
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
            [ testColorSerDe 0 "000000"
            , testColorSerDe 15 "00000f"
            , testColorSerDe 255 "0000ff"
            , testColorSerDe 4095 "000fff"
            , testColorSerDe 65535 "00ffff"
            , testColorSerDe 1048575 "0fffff"
            , testColorSerDe 16777215 "ffffff"
            ]
        ]


testColorSerDe : Int -> String -> Test
testColorSerDe colorNum expectedHex =
    test ("should decode/encode color integer " ++ fromInt colorNum) <|
        \_ ->
            let
                expectedColor =
                    Result.map3 rgb255
                        (expectedHex |> String.slice 0 2 |> Hex.fromString)
                        (expectedHex |> String.slice 2 4 |> Hex.fromString)
                        (expectedHex |> String.slice 4 6 |> Hex.fromString)
                        |> Result.withDefault (rgb255 0 0 0)
            in
            colorNum
                |> fromInt
                |> decodeString Data.Producer.Discord.colorDecoder
                |> Result.map (Json.EncodeExtra.color >> encode 0)
                |> Result.andThen (decodeString Data.Producer.Discord.colorDecoder)
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
            (D.field "channels" (D.list Slack.conversationDecoder))
            (Json.Encode.list Slack.encodeConversation)
            (D.list Slack.conversationDecoder)
        , test "should decode Message list" <|
            -- TODO: encode
            \_ ->
                SlackTestData.conversationHistoryJson
                    |> decodeString (D.field "messages" (D.list Slack.messageDecoder))
                    |> Expect.ok
        , let
            pub name isMember =
                Slack.PublicChannel
                    { id = Slack.dummyConversationId, name = name, isMember = isMember, lastRead = Nothing, fetchStatus = Available }

            priv name =
                Slack.PrivateChannel
                    { id = Slack.dummyConversationId, name = name, lastRead = Nothing, fetchStatus = Available }

            im id =
                Slack.IM
                    { id = Slack.dummyConversationId, user = Slack.dummyUserId id, lastRead = Nothing, fetchStatus = Available }

            mpim name =
                Slack.MPIM
                    { id = Slack.dummyConversationId, name = name, lastRead = Nothing, fetchStatus = Available }
          in
          describe "compareByMembersipThenName"
            [ testCompareConversation (pub "Name" True) (pub "Aaaa" True) GT
            , testCompareConversation (pub "Name" True) (pub "Name" True) EQ
            , testCompareConversation (pub "Name" True) (pub "Zzzz" True) LT
            , testCompareConversation (pub "Name" True) (priv "Name") LT
            , testCompareConversation (pub "Name" True) (im "USER") LT
            , testCompareConversation (pub "Name" True) (mpim "Users") LT
            , testCompareConversation (pub "Name" True) (pub "Aaaa" False) LT
            , testCompareConversation (pub "Name" True) (pub "Name" False) LT
            , testCompareConversation (pub "Name" True) (pub "Zzzz" False) LT
            , testCompareConversation (priv "Name") (pub "Name" True) GT
            , testCompareConversation (priv "Name") (priv "Aaaa") GT
            , testCompareConversation (priv "Name") (priv "Name") EQ
            , testCompareConversation (priv "Name") (priv "Zzzz") LT
            , testCompareConversation (priv "Name") (im "USER") LT
            , testCompareConversation (priv "Name") (mpim "Users") LT
            , testCompareConversation (priv "Name") (pub "Name" False) LT
            , testCompareConversation (im "USER") (pub "Name" True) GT
            , testCompareConversation (im "USER") (priv "Name") GT
            , testCompareConversation (im "USER") (im "AAAA") GT
            , testCompareConversation (im "USER") (im "USER") EQ
            , testCompareConversation (im "USER") (im "ZZZZ") LT
            , testCompareConversation (im "USER") (mpim "Users") LT
            , testCompareConversation (im "USER") (pub "Name" False) LT
            , testCompareConversation (mpim "Users") (pub "Name" True) GT
            , testCompareConversation (mpim "Users") (priv "Name") GT
            , testCompareConversation (mpim "Users") (im "USER") GT
            , testCompareConversation (mpim "Users") (mpim "Aaaaa") GT
            , testCompareConversation (mpim "Users") (mpim "Users") EQ
            , testCompareConversation (mpim "Users") (mpim "Zzzzz") LT
            , testCompareConversation (mpim "Users") (pub "Name" False) LT
            , testCompareConversation (pub "Name" False) (pub "Aaaa" True) GT
            , testCompareConversation (pub "Name" False) (pub "Name" True) GT
            , testCompareConversation (pub "Name" False) (pub "Zzzz" True) GT
            , testCompareConversation (pub "Name" False) (priv "Name") GT
            , testCompareConversation (pub "Name" False) (im "USER") GT
            , testCompareConversation (pub "Name" False) (mpim "Users") GT
            , testCompareConversation (pub "Name" False) (pub "Aaaa" False) GT
            , testCompareConversation (pub "Name" False) (pub "Name" False) EQ
            , testCompareConversation (pub "Name" False) (pub "Zzzz" False) LT
            ]
        ]


testCodec : String -> String -> Decoder a -> (a -> Value) -> Decoder a -> Test
testCodec desc initialData entryDecoder encodeForPersist savedStateDecoder =
    test desc <|
        \_ ->
            case decodeString entryDecoder initialData of
                Ok enteredData ->
                    enteredData
                        |> encodeForPersist
                        |> encode 0
                        |> decodeString savedStateDecoder
                        -- Must exactly match after state recovery
                        |> Expect.equal (Ok enteredData)

                Err e ->
                    Expect.fail <| "Failed to decode on entry: " ++ D.errorToString e


testCompareConversation : Slack.Conversation -> Slack.Conversation -> Order -> Test
testCompareConversation a b order =
    test ("'" ++ Debug.toString a ++ "' " ++ Debug.toString order ++ " '" ++ Debug.toString b ++ "'") <|
        \_ ->
            Slack.compareByMembersipThenName a b |> Expect.equal order
