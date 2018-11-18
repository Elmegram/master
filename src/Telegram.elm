module Telegram exposing
    ( Chat
    , ChatType(..)
    , Message
    , SendMessage
    , Update
    , UpdateContent(..)
    , UpdateId
    , User
    , decodeUpdate
    , decodeUser
    , encodeSendMessage
    , makeTestId
    )

import Json.Decode as Decode
import Json.Encode as Encode



-- UPDATE


type alias Update =
    { update_id : Id UpdateId
    , content : UpdateContent
    }


type UpdateId
    = UpdateId


type UpdateContent
    = MessageUpdate Message


decodeUpdate : Decode.Decoder Update
decodeUpdate =
    Decode.map2
        Update
        (Decode.field "update_id" Decode.int |> Decode.map Id)
        (Decode.field "message" decodeMessage |> Decode.map MessageUpdate)


type alias Message =
    { message_id : Id MessageTag
    , date : Int
    , chat : Chat
    , text : String
    }


type MessageTag
    = MessageTag


decodeMessage : Decode.Decoder Message
decodeMessage =
    Decode.map4
        Message
        (Decode.field "message_id" Decode.int |> Decode.map Id)
        (Decode.field "date" Decode.int)
        (Decode.field "chat" decodeChat)
        (Decode.field "text" Decode.string)


type alias Chat =
    { id : Id ChatTag
    , type_ : ChatType
    }


type ChatTag
    = ChatTag


type ChatType
    = Private
    | Group
    | Supergroup
    | Channel


decodeChat : Decode.Decoder Chat
decodeChat =
    Decode.map2
        Chat
        (Decode.field "id" Decode.int |> Decode.map Id)
        (Decode.field "type" Decode.string
            |> Decode.andThen
                (\typeString ->
                    case typeString of
                        "private" ->
                            Decode.succeed Private

                        "group" ->
                            Decode.succeed Group

                        "supergroup" ->
                            Decode.succeed Supergroup

                        "channel" ->
                            Decode.succeed Channel

                        _ ->
                            Decode.fail ("Chat type " ++ typeString ++ " is not known.")
                )
        )


type alias User =
    { id : Id UserTag
    , is_bot : Bool
    , first_name : String
    , last_name : Maybe String
    , username : Maybe String
    , language_code : Maybe String
    }


type UserTag
    = UserTag


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map6
        User
        (Decode.field "id" Decode.int |> Decode.map Id)
        (Decode.field "is_bot" Decode.bool)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" (Decode.maybe Decode.string))
        (Decode.field "username" (Decode.maybe Decode.string))
        (Decode.field "language_code" (Decode.maybe Decode.string))


type Id a
    = Id Int


encodeId : Id a -> Encode.Value
encodeId (Id id) =
    Encode.int id



-- RESPONSE


type alias SendMessage =
    { chat_id : Id ChatTag
    , text : String
    }


encodeSendMessage : SendMessage -> Encode.Value
encodeSendMessage sendMessage =
    Encode.object
        [ ( "chat_id", encodeId sendMessage.chat_id )
        , ( "text", Encode.string sendMessage.text )
        ]



-- TEST


makeTestId : Int -> Id a
makeTestId id =
    Id id
