module Elmergram exposing (answer, decodeUpdate, encodeSendMessage)

import Json.Decode as Decode
import Json.Encode as Encode
import Telegram



-- I/O


decodeUpdate : Encode.Value -> Result Decode.Error Telegram.Update
decodeUpdate =
    Decode.decodeValue Telegram.decodeUpdate


encodeSendMessage : Telegram.SendMessage -> Encode.Value
encodeSendMessage =
    Telegram.encodeSendMessage



-- UTILITIES


answer : String -> Telegram.Chat -> Telegram.SendMessage
answer text chat =
    { chat_id = chat.id
    , text = text
    }
