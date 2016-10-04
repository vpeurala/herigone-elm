module NonemptyUtil exposing (nonempty, shuffle)

import Random exposing (Generator, Generator(..), Seed)
import List.Nonempty as NE exposing (Nonempty, (:::))


nonempty : Int -> Generator a -> Generator (Nonempty a)
nonempty n generator =
    if (n < 1) then
        Debug.crash ("Smaller than 1 passed as length to nonempty list generator: " ++ (toString n))
    else
        Generator <| \seed -> nonemptyHelp [] n generate seed


nonemptyHelp : List a -> Int -> ( Seed, ( a, Seed ) ) -> Seed -> ( List a, Seed )
nonemptyHelp list n generate seed =
    if (n < 1) then
        ( NE.reverse list, seed )
    else
        let
            ( value, newSeed ) =
                generate seed
        in
            nonemptyHelp (value ::: list) (n - 1) generate newSeed


shuffle : Nonempty a -> Generator (Nonempty a)
shuffle input =
    Debug.crash "shuffle"
