module Elmergram exposing (ErrorPort, UpdateHandler, UpdateResult, answer, decodeUpdate, encodeSendMessage, processUpdate)

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


type alias UpdateResult =
    Result Decode.Error Telegram.Update


type alias ErrorPort msg =
    String -> Cmd msg


type alias UpdateHandler msg model =
    Telegram.Update -> model -> ( model, Cmd msg )


processUpdate : ErrorPort msg -> UpdateHandler msg model -> UpdateResult -> model -> ( model, Cmd msg )
processUpdate error handleUpdate result model =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> error )

        Ok newUpdate ->
            handleUpdate newUpdate model



-- UTILITIES


answer : String -> Telegram.Chat -> Telegram.SendMessage
answer text chat =
    { chat_id = chat.id
    , text = text
    }