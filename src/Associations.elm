module Associations exposing (..)

-- OS

import List.Nonempty exposing (Nonempty)
import List.Nonempty as Nonempty


-- Code


type alias Association =
    { number : String
    , word : String
    }


allAssociations : Nonempty Association
allAssociations =
    case
        Nonempty.fromList
            [ { number = "0", word = "hai" }
            , { number = "1", word = "jää" }
            , { number = "2", word = "kuu" }
            , { number = "3", word = "luu" }
            , { number = "4", word = "maa" }
            , { number = "5", word = "puu" }
            , { number = "6", word = "rae" }
            , { number = "7", word = "suu" }
            , { number = "8", word = "täi" }
            , { number = "9", word = "voi" }
            , { number = "00", word = "hiha" }
            , { number = "01", word = "häjy" }
            , { number = "02", word = "hauki" }
            , { number = "03", word = "huilu" }
            , { number = "04", word = "haamu" }
            , { number = "05", word = "huopa" }
            , { number = "06", word = "hiiri" }
            , { number = "07", word = "huusi" }
            , { number = "08", word = "hauta" }
            , { number = "09", word = "haavi" }
            , { number = "10", word = "jauho" }
            , { number = "11", word = "jojo" }
            , { number = "12", word = "joki" }
            , { number = "13", word = "joulu" }
            , { number = "14", word = "juomu" }
            , { number = "15", word = "jopo" }
            , { number = "16", word = "juuri" }
            , { number = "17", word = "jousi" }
            , { number = "18", word = "jeti" }
            , { number = "19", word = "jyvä" }
            , { number = "20", word = "koho" }
            , { number = "21", word = "koju" }
            , { number = "22", word = "keko" }
            , { number = "23", word = "kela" }
            , { number = "24", word = "kuomu" }
            , { number = "25", word = "kupu" }
            , { number = "26", word = "koira" }
            , { number = "27", word = "kaasu" }
            , { number = "28", word = "kota" }
            , { number = "29", word = "kavio" }
            , { number = "30", word = "liha" }
            , { number = "31", word = "lyijy" }
            , { number = "32", word = "laki" }
            , { number = "33", word = "luola" }
            , { number = "34", word = "liima" }
            , { number = "35", word = "lapio" }
            , { number = "36", word = "luuri" }
            , { number = "37", word = "liesi" }
            , { number = "38", word = "luoti" }
            , { number = "39", word = "laiva" }
            , { number = "40", word = "maha" }
            , { number = "41", word = "maja" }
            , { number = "42", word = "muki" }
            , { number = "43", word = "mela" }
            , { number = "44", word = "muumi" }
            , { number = "45", word = "mopo" }
            , { number = "46", word = "muuri" }
            , { number = "47", word = "muusi" }
            , { number = "48", word = "mato" }
            , { number = "49", word = "muovi" }
            , { number = "50", word = "pyyhe" }
            , { number = "51", word = "poiju" }
            , { number = "52", word = "puku" }
            , { number = "53", word = "pulu" }
            , { number = "54", word = "puuma" }
            , { number = "55", word = "pipo" }
            , { number = "56", word = "pora" }
            , { number = "57", word = "puosu" }
            , { number = "58", word = "pata" }
            , { number = "59", word = "paavi" }
            , { number = "60", word = "raha" }
            , { number = "61", word = "ryijy" }
            , { number = "62", word = "reki" }
            , { number = "63", word = "railo" }
            , { number = "64", word = "riimu" }
            , { number = "65", word = "rapu" }
            , { number = "66", word = "ruori" }
            , { number = "67", word = "ruusu" }
            , { number = "68", word = "ruoto" }
            , { number = "69", word = "rovio" }
            , { number = "70", word = "saha" }
            , { number = "71", word = "soija" }
            , { number = "72", word = "sika" }
            , { number = "73", word = "siili" }
            , { number = "74", word = "siima" }
            , { number = "75", word = "siipi" }
            , { number = "76", word = "saari" }
            , { number = "77", word = "susi" }
            , { number = "78", word = "suti" }
            , { number = "79", word = "sauva" }
            , { number = "80", word = "tuohi" }
            , { number = "81", word = "taiji" }
            , { number = "82", word = "tiuku" }
            , { number = "83", word = "tuoli" }
            , { number = "84", word = "taimi" }
            , { number = "85", word = "tipu" }
            , { number = "86", word = "tiira" }
            , { number = "87", word = "teesi" }
            , { number = "88", word = "toti" }
            , { number = "89", word = "tavi" }
            , { number = "90", word = "vuohi" }
            , { number = "91", word = "vaja" }
            , { number = "92", word = "vaaka" }
            , { number = "93", word = "viulu" }
            , { number = "94", word = "vaimo" }
            , { number = "95", word = "vapa" }
            , { number = "96", word = "viiri" }
            , { number = "97", word = "vaasi" }
            , { number = "98", word = "vouti" }
            , { number = "99", word = "vauva" }
            ]
    of
        Nothing ->
            Debug.crash "This is impossible!"

        Just x ->
            x
