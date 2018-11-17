module StringFuzzer exposing (nonblankStringFuzzer, nonemptyStringFuzzer, whitespaceStringFuzzer)

import Fuzz exposing (Fuzzer)
import Random
import Random.Char
import Random.Extra as Random
import Random.String
import Shrink


nonemptyStringFuzzer : Fuzzer String
nonemptyStringFuzzer =
    Fuzz.custom
        nonemptyGenerator
        (Shrink.string |> Shrink.keepIf (\string -> String.length string > 0))


lengthGenerator : Random.Generator Int
lengthGenerator =
    Random.frequency
        ( 3, Random.int 1 10 )
        [ ( 1, Random.int 11 50 )
        , ( 1, Random.int 50 1000 )
        ]


nonemptyGenerator : Random.Generator String
nonemptyGenerator =
    lengthGenerator
        |> Random.andThen (\length -> Random.String.string length Random.Char.english)


{-| Generates nonempty strings that contain at least one printable (i. e. not whitespace) character.
-}
nonblankStringFuzzer : Fuzzer String
nonblankStringFuzzer =
    Fuzz.custom
        nonblankGenerator
        (Shrink.string |> Shrink.keepIf (\string -> String.length string > 0))


nonblankGenerator : Random.Generator String
nonblankGenerator =
    lengthGenerator
        |> Random.andThen
            (\length ->
                let
                    partLength =
                        length // 2 - 1

                    nonblanks =
                        Random.choices Random.Char.english []

                    build : String -> Char -> String -> String
                    build front nonblank back =
                        front ++ String.fromChar nonblank ++ back
                in
                Random.map3
                    build
                    (Random.String.string partLength nonblanks)
                    nonblanks
                    (Random.String.string partLength nonblanks)
            )


{-| Generates strings containing only whitespace.

The empty string is also produced.

-}
whitespaceStringFuzzer : Fuzzer String
whitespaceStringFuzzer =
    Fuzz.custom
        whitespaceGenerator
        Shrink.string


whitespaceGenerator : Random.Generator String
whitespaceGenerator =
    Random.frequency
        ( 9, lengthGenerator )
        [ ( 1, Random.constant 0 )
        ]
        |> Random.andThen (\length -> Random.String.string length whitespace)


whitespace : Random.Generator Char
whitespace =
    let
        space =
            32

        tab =
            9
    in
    Random.choices (Random.constant space) [ Random.constant tab ]
        |> Random.map Char.fromCode
