module EchoBot exposing (handle)

import Elmergram
import Telegram


handle : Telegram.Update -> Telegram.SendMessage
handle newUpdate =
    case newUpdate.content of
        Telegram.MessageUpdate message ->
            if String.contains "hi" (String.toLower message.text) then
                Elmergram.answer "Hi!" message.chat

            else if String.contains "bye" (String.toLower message.text) then
                Elmergram.answer "Bye!" message.chat

            else
                Elmergram.answer message.text message.chat
