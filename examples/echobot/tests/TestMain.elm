module TestMain exposing (suite)

import Elmegram.Bot as Elmegram
import Expect
import Main exposing (bot)
import Telegram
import Telegram.Test as TeleTest
import Test exposing (..)


suite : Test
suite =
    test "repeats incoming text message" <|
        \_ ->
            let
                init =
                    bot.init TeleTest.makeUser

                update =
                    TeleTest.makeMessage "echo me" |> TeleTest.send
            in
            bot.update (bot.newUpdateMsg update) init.model
                |> .methods
                |> List.any
                    (\method ->
                        case method of
                            Elmegram.SendMessageMethod sendMessage ->
                                "You said: echo me" == sendMessage.text

                            _ ->
                                False
                    )
                |> Expect.true "No text message containing 'You said: echo me' was sent."
