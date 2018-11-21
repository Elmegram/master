module Elmegram exposing
    ( Response
    , answer
    , answerFormatted
    , format
    , getName
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Telegram



-- INTERFACE


type alias Response model msg =
    { messages : List Telegram.SendMessage
    , model : model
    , command : Cmd msg
    }



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
