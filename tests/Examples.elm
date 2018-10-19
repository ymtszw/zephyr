module Examples exposing (suite)

import Array exposing (fromList)
import Data.Array as Array
import Data.Column
import Data.Item
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Data.TextRenderer exposing (StringOrUrl(..))
import Data.UniqueId exposing (Generator)
import Expect exposing (Expectation)
import Fuzz
import Json.Decode exposing (decodeValue)
import Parser
import String exposing (fromInt)
import Test exposing (..)
import Time exposing (Posix)
import Url



-- Data.Array


testSplitAt : List a -> Int -> ( List a, List a ) -> Test
testSplitAt initial index ( expectFront, expectRear ) =
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


arraySuite : Test
arraySuite =
    describe "Data.Array"
        [ describe "splitAt"
            [ testSplitAt [ 0, 1, 2 ] 0 ( [], [ 0, 1, 2 ] )
            , testSplitAt [ 0, 1, 2 ] 1 ( [ 0 ], [ 1, 2 ] )
            , testSplitAt [ 0, 1, 2 ] 2 ( [ 0, 1 ], [ 2 ] )
            , testSplitAt [ 0, 1, 2 ] 3 ( [ 0, 1, 2 ], [] )
            , testSplitAt [ 0, 1, 2 ] 4 ( [ 0, 1, 2 ], [] )
            , testSplitAt [ 0, 1, 2 ] -1 ( [ 0, 1 ], [ 2 ] )
            , testSplitAt [ 0, 1, 2 ] -2 ( [ 0 ], [ 1, 2 ] )
            , testSplitAt [ 0, 1, 2 ] -3 ( [], [ 0, 1, 2 ] )
            , testSplitAt [ 0, 1, 2 ] -4 ( [], [ 0, 1, 2 ] )
            , testSplitAt [] 0 ( [], [] )
            , testSplitAt [] 1 ( [], [] )
            , testSplitAt [] -1 ( [], [] )
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
        , let
            isEven i =
                modBy 2 i == 0
          in
          describe "all"
            [ testAll [ 0, 2, 4 ] isEven True
            , testAll [ 0 ] isEven True
            , testAll [] isEven True
            , testAll [ 0, 1, 2 ] isEven False
            ]
        ]



-- Data.UniqueId


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
                    genAll requests Data.UniqueId.init
            in
            Expect.equalLists expects actuals


genAll : List ( String, Int ) -> Generator -> ( List String, Generator )
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


seqGen : String -> Int -> Generator -> ( String, Generator )
seqGen prefix howMany generator =
    seqGenImpl prefix howMany ( "not generated", generator )


seqGenImpl : String -> Int -> ( String, Generator ) -> ( String, Generator )
seqGenImpl prefix howMany ( lastResult, accGenerator ) =
    if howMany <= 0 then
        ( lastResult, accGenerator )

    else
        seqGenImpl prefix (howMany - 1) (Data.UniqueId.gen prefix accGenerator)


uniqueIdSuite : Test
uniqueIdSuite =
    describe "Data.UniqueId"
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



-- Data.Item


itemSuite : Test
itemSuite =
    describe "Data.Item"
        [ test "encoder/decoder should work" <|
            \_ ->
                Data.Item.welcome
                    |> Data.Item.encode
                    |> decodeValue Data.Item.decoder
                    |> Expect.equal (Ok Data.Item.welcome)
        ]



-- Data.Column


columnSuite : Test
columnSuite =
    describe "Data.Column"
        [ test "encoder/decoder should work" <|
            \_ ->
                let
                    welcome =
                        Data.Column.welcome "test"
                in
                welcome
                    |> Data.Column.encode
                    |> decodeValue Data.Column.decoder
                    |> Expect.equal (Ok { welcome | configOpen = False })
        ]



-- Data.TextRenderer


testParseIntoStringOrUrlList : String -> List StringOrUrl -> Test
testParseIntoStringOrUrlList string expect =
    test ("should work for text: '" ++ string ++ "'") <|
        \_ ->
            string
                |> Parser.run Data.TextRenderer.parseIntoStringOrUrlList
                |> Expect.equal (Ok expect)


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


exampleCom : Url.Url
exampleCom =
    { protocol = Url.Http
    , host = "example.com"
    , port_ = Nothing
    , path = "/"
    , fragment = Nothing
    , query = Nothing
    }



-- Data.Producer.FetchStatus


fetchStatusSuite : Test
fetchStatusSuite =
    describe "Data.Producer.FetchStatus"
        [ describe "compare"
            [ testCompare NeverFetched NeverFetched EQ
            , testCompare NeverFetched Waiting LT
            , testCompare NeverFetched (NextFetchAt (p 1) BO5) LT
            , testCompare NeverFetched InitialFetching LT
            , testCompare NeverFetched (Fetching (p 1) BO5) LT
            , testCompare NeverFetched Available LT
            , testCompare NeverFetched Forbidden LT
            , testCompare Waiting NeverFetched GT
            , testCompare Waiting Waiting EQ
            , testCompare Waiting (NextFetchAt (p 1) BO5) LT
            , testCompare Waiting InitialFetching LT
            , testCompare Waiting (Fetching (p 1) BO5) LT
            , testCompare Waiting Available LT
            , testCompare Waiting Forbidden LT
            , testCompare (NextFetchAt (p 1) BO5) NeverFetched GT
            , testCompare (NextFetchAt (p 1) BO5) Waiting GT
            , testCompare (NextFetchAt (p 1) BO5) (NextFetchAt (p 0) BO5) GT
            , testCompare (NextFetchAt (p 1) BO5) (NextFetchAt (p 1) BO5) EQ
            , testCompare (NextFetchAt (p 1) BO5) (NextFetchAt (p 1) BO10) EQ
            , testCompare (NextFetchAt (p 1) BO5) (NextFetchAt (p 2) BO5) LT
            , testCompare (NextFetchAt (p 1) BO5) InitialFetching LT
            , testCompare (NextFetchAt (p 1) BO5) (Fetching (p 1) BO5) LT
            , testCompare (NextFetchAt (p 1) BO5) Available LT
            , testCompare (NextFetchAt (p 1) BO5) Forbidden LT
            , testCompare InitialFetching NeverFetched GT
            , testCompare InitialFetching Waiting GT
            , testCompare InitialFetching (NextFetchAt (p 1) BO5) GT
            , testCompare InitialFetching InitialFetching EQ
            , testCompare InitialFetching (Fetching (p 1) BO5) LT
            , testCompare InitialFetching Available LT
            , testCompare InitialFetching Forbidden LT
            , testCompare (Fetching (p 1) BO5) NeverFetched GT
            , testCompare (Fetching (p 1) BO5) Waiting GT
            , testCompare (Fetching (p 1) BO5) (NextFetchAt (p 1) BO5) GT
            , testCompare (Fetching (p 1) BO5) InitialFetching GT
            , testCompare (Fetching (p 1) BO5) (Fetching (p 0) BO5) GT
            , testCompare (Fetching (p 1) BO5) (Fetching (p 1) BO5) EQ
            , testCompare (Fetching (p 1) BO5) (Fetching (p 1) BO10) EQ
            , testCompare (Fetching (p 1) BO5) (Fetching (p 2) BO5) LT
            , testCompare (Fetching (p 1) BO5) Available LT
            , testCompare (Fetching (p 1) BO5) Forbidden LT
            , testCompare Available NeverFetched GT
            , testCompare Available Waiting GT
            , testCompare Available (NextFetchAt (p 1) BO5) GT
            , testCompare Available InitialFetching GT
            , testCompare Available (Fetching (p 1) BO5) GT
            , testCompare Available Available EQ
            , testCompare Available Forbidden LT
            , testCompare Forbidden NeverFetched GT
            , testCompare Forbidden Waiting GT
            , testCompare Forbidden (NextFetchAt (p 1) BO5) GT
            , testCompare Forbidden InitialFetching GT
            , testCompare Forbidden (Fetching (p 1) BO5) GT
            , testCompare Forbidden Available GT
            , testCompare Forbidden Forbidden EQ
            ]
        , describe "lessThan"
            [ NeverFetched |> testLessThan (NextFetchAt (p 1) BO5)
            , NextFetchAt (p 0) BO5 |> testLessThan (NextFetchAt (p 1) BO5)
            ]
        ]


p : Int -> Posix
p =
    Time.millisToPosix


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



-- MAIN


suite : Test
suite =
    describe "test"
        [ arraySuite
        , uniqueIdSuite
        , itemSuite
        , columnSuite
        , textRendererSuite
        , fetchStatusSuite
        ]
