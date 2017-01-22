module NonemptyUtil exposing (nonempty, shuffle)

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
            nonempty len (Random.int 0 (len * 1000))

        zips : Generator (Nonempty ( a, Int ))
        zips =
            Random.map (\rs -> Nonempty.map2 (,) input rs) randoms

        sorted : Generator (Nonempty ( a, Int ))
        sorted =
            Random.map (Nonempty.sortBy Tuple.second) zips

        vals : Generator (Nonempty a)
        vals =
            Random.map (\pairs -> Nonempty.map Tuple.first pairs) sorted
    in
        vals


nonempty : Int -> Generator a -> Generator (Nonempty a)
nonempty len gen =
    Random.andThen
        (\head ->
            let
                tail =
                    Random.list (len - 1) gen
            in
                Random.map (Nonempty.Nonempty head) tail
        )
        gen
