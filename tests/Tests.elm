module Tests exposing (..)

import NonemptyUtil

import Test exposing (..)
import Expect
import String

import Random exposing (int)


all : Test
all =
    describe "NonemptyUtil"
        [ test "Can generate" <|
            \() ->
                Expect.equal (NonemptyUtil.nonempty 3 (int 0 10)) [1, 2, 3]
        ]
