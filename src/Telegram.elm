module Telegram exposing
    ( Chat
    , ChatType(..)
    , Message
    , SendMessage
    , Update
    , UpdateContent(..)
    , UpdateId
    , decodeUpdate
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


type alias Message =
    { message_id : Id MessageTag
    , date : Int
    , chat : Chat
    , text : String
    }


type MessageTag
    = MessageTag


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


type Id a
    = Id Int


encodeId : Id a -> Encode.Value
encodeId (Id id) =
    Encode.int id


decodeUpdate : Decode.Decoder Update
decodeUpdate =
    Decode.map2
        Update
        (Decode.field "update_id" Decode.int |> Decode.map Id)
        (Decode.field "message" decodeMessage |> Decode.map MessageUpdate)


decodeMessage : Decode.Decoder Message
decodeMessage =
    Decode.map4
        Message
        (Decode.field "message_id" Decode.int |> Decode.map Id)
        (Decode.field "date" Decode.int)
        (Decode.field "chat" decodeChat)
        (Decode.field "text" Decode.string)


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
