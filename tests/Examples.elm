module Examples exposing (suite)

import Array exposing (fromList)
import Data.Array as Array
import Data.Column
import Data.Item
import Data.UniqueId exposing (Generator)
import Expect exposing (Expectation)
import Fuzz
import Json.Decode exposing (decodeValue)
import String exposing (fromInt)
import Test exposing (..)
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


arraySuites : Test
arraySuites =
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


uniqueIdSuites : Test
uniqueIdSuites =
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


itemSuites : Test
itemSuites =
    describe "Data.Item"
        [ test "encoder/decoder should work" <|
            \_ ->
                Data.Item.welcome
                    |> Data.Item.encoder
                    |> decodeValue Data.Item.decoder
                    |> Expect.equal (Ok Data.Item.welcome)
        ]



-- Data.Column


columnSuites : Test
columnSuites =
    describe "Data.Column"
        [ test "encoder/decoder should work" <|
            \_ ->
                Data.Column.welcome "test"
                    |> Data.Column.encoder
                    |> decodeValue Data.Column.decoder
                    |> Expect.equal (Ok (Data.Column.welcome "test"))
        ]



-- MAIN


suite : Test
suite =
    describe "test"
        [ arraySuites
        , uniqueIdSuites
        , itemSuites
        , columnSuites
        ]
