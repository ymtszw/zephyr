module Examples exposing (suite)

import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "String"
        [ fuzz Fuzz.string "reverse" <| \s -> s |> String.reverse |> String.reverse |> Expect.equal (s ++ s)
        ]
