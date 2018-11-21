module Elmegram exposing
    ( ErrorPort
    , UpdateHandler
    , UpdateResult
    , answer
    , answerFormatted
    , decodeUpdate
    , encodeSendMessage
    , format
    , getName
    , processUpdate
    )

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



-- MESSAGES


answer : Telegram.Chat -> String -> Telegram.SendMessage
answer chat text =
    { chat_id = chat.id
    , text = text
    , parse_mode = Nothing
    }


answerFormatted : Telegram.Chat -> FormattedText -> Telegram.SendMessage
answerFormatted chat (Format mode text) =
    { chat_id = chat.id
    , text = text
    , parse_mode = Just mode
    }


type FormattedText
    = Format Telegram.ParseMode String


format : Telegram.ParseMode -> String -> FormattedText
format mode text =
    Format mode text



-- USERS


getName : Telegram.User -> String
getName user =
    case user.username of
        Just username ->
            username

        Nothing ->
            case user.last_name of
                Just lastName ->
                    user.first_name ++ " " ++ lastName

                Nothing ->
                    user.first_name
