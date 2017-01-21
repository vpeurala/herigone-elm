module Tests exposing (..)

import NonemptyUtil
import Test exposing (..)
import Expect
import String
import Random
import List.Nonempty as Nonempty


all : Test
all =
    describe "NonemptyUtil"
        [ test "Can generate" <|
            \() ->
                let
                    generator =
                        (NonemptyUtil.nonempty 3 (Random.int 0 10))
                in
                    Expect.equal (Tuple.first (Random.step generator (Random.initialSeed 0)))
                        (Nonempty.Nonempty 0 [ 3, 6 ])
        , test "Can shuffle" <|
            \() ->
                Expect.equal (Tuple.first (Random.step (NonemptyUtil.shuffle (Nonempty.Nonempty 1 [ 2, 3 ])) (Random.initialSeed 0)))
                    (Nonempty.Nonempty 2 [ 1, 3 ])
        ]
