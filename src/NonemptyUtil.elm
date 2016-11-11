module NonemptyUtil exposing (shuffle)

import Random exposing (Generator)
import List.Nonempty as Nonempty exposing (Nonempty)


shuffle : Nonempty a -> Generator (Nonempty a)
shuffle input =
    let
        len : Int
        len =
            Nonempty.length input

        randoms : Generator (Nonempty Int)
        randoms =
            nonempty len (Random.int 0 10000)

        zips : Generator (Nonempty ( Int, Int ))
        zips =
            Random.andThen randoms (\rs -> Nonempty.map2 (,) input rs)

        sorted : Generator (Nonempty ( Int, Int ))
        sorted =
            Random.andThen zips (Nonempty.sortBy snd)
    in
        Nonempty.sortBy snd zips |> Nonempty.map fst


nonempty : Int -> Generator a -> Generator (Nonempty a)
nonempty len gen =
    let
        ( head, seed' ) =
            Random.step gen (Random.initialSeed 0)

        tail =
            Random.list (len - 1) gen
    in
        Random.map (\tail -> Nonempty.Nonempty head tail) tail
