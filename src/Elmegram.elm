module Elmegram exposing
    ( Response
    , answer
    , answerFormatted
    , containsCommand
    , format
    , getDisplayName
    , matchesCommand
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


containsCommand : Telegram.TextMessage -> Bool
containsCommand message =
    List.any
        (\entity ->
            case entity of
                Telegram.BotCommand _ ->
                    True

                _ ->
                    False
        )
        message.entities


matchesCommand : String -> Telegram.TextMessage -> Bool
matchesCommand command message =
    List.any
        (\entity ->
            case entity of
                Telegram.BotCommand bounds ->
                    let
                        end =
                            bounds.offset + bounds.length
                    in
                    -- Drop the '/'.
                    String.dropLeft 1 message.text
                        |> String.slice bounds.offset end
                        |> String.split "@"
                        |> List.head
                        |> Maybe.map (\actual -> actual == command)
                        |> Maybe.withDefault False

                _ ->
                    False
        )
        message.entities



-- SEND MESSAGES


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


getDisplayName : Telegram.User -> String
getDisplayName user =
    case user.username of
        Just username ->
            username

        Nothing ->
            case user.last_name of
                Just lastName ->
                    user.first_name ++ " " ++ lastName

                Nothing ->
                    user.first_name
