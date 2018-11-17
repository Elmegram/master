module TestEcho exposing (suite)

import EchoBot
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import StringFuzzer exposing (nonemptyStringFuzzer)
import Telegram.Test as Telegram
import Test exposing (..)


suite : Test
suite =
    describe "Echo bot"
        [ fuzz nonemptyStringFuzzer "responds with the same text" <|
            \messageText ->
                let
                    update =
                        Telegram.sendMessage messageText
                in
                EchoBot.handle update
                    |> .text
                    |> Expect.equal messageText
        ]
