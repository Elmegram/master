module Telegram exposing
    ( Bounds
    , Chat
    , ChatType(..)
    , Message
    , MessageEntity(..)
    , ParseMode(..)
    , SendMessage
    , Update
    , UpdateContent(..)
    , UpdateId
    , User
    , decodeBounds
    , decodeChat
    , decodeMessageEntity
    , decodeUpdate
    , decodeUser
    , encodeSendMessage
    , makeTestId
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)



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
    , entities : List MessageEntity
    }


type MessageTag
    = MessageTag


type MessageEntity
    = Mention Bounds
    | Hashtag Bounds
    | Cashtag Bounds
    | BotCommand Bounds
    | Url Bounds
    | Email Bounds
    | PhoneNumber Bounds
    | Bold Bounds
    | Italic Bounds
    | Code Bounds
    | Pre Bounds
    | TextLink Bounds Url
    | TextMention Bounds User


type alias Bounds =
    { offset : Int
    , length : Int
    }


decodeBounds : Decode.Decoder Bounds
decodeBounds =
    Decode.map2
        Bounds
        (Decode.field "offset" Decode.int)
        (Decode.field "length" Decode.int)


decodeMessageEntity : Decode.Decoder MessageEntity
decodeMessageEntity =
    let
        simple =
            Decode.map2
                (\type_ bounds ->
                    ( type_, bounds )
                )
                (Decode.field "type" Decode.string)
                decodeBounds
                |> Decode.andThen
                    (\( type_, bounds ) ->
                        case type_ of
                            "mention" ->
                                Decode.succeed (Mention bounds)

                            "hashtag" ->
                                Decode.succeed (Hashtag bounds)

                            "cashtag" ->
                                Decode.succeed (Cashtag bounds)

                            "bot_command" ->
                                Decode.succeed (BotCommand bounds)

                            "url" ->
                                Decode.succeed (Url bounds)

                            "email" ->
                                Decode.succeed (Email bounds)

                            "phone_number" ->
                                Decode.succeed (PhoneNumber bounds)

                            "bold" ->
                                Decode.succeed (Bold bounds)

                            "italic" ->
                                Decode.succeed (Italic bounds)

                            "code" ->
                                Decode.succeed (Code bounds)

                            "pre" ->
                                Decode.succeed (Pre bounds)

                            wrongType ->
                                Decode.fail
                                    ("Expected a simple type, but the field 'type' contained '"
                                        ++ wrongType
                                        ++ "'."
                                    )
                    )

        textLink =
            Decode.map3
                (\type_ bounds url -> ( type_, bounds, url ))
                (Decode.field "type" Decode.string)
                decodeBounds
                (Decode.field "url" Decode.string)
                |> Decode.andThen
                    (\( type_, bounds, urlString ) ->
                        case Url.fromString urlString of
                            Just url ->
                                if type_ == "text_link" then
                                    Decode.succeed (TextLink bounds url)

                                else
                                    Decode.fail
                                        ("Expected field 'type' to be 'text_link', but it was '"
                                            ++ type_
                                            ++ "'."
                                        )

                            Nothing ->
                                Decode.fail
                                    ("Expected field 'url' to contain a valid URL, but it was '"
                                        ++ urlString
                                        ++ "'."
                                    )
                    )

        textMention =
            Decode.map3
                (\type_ bounds user -> ( type_, bounds, user ))
                (Decode.field "type" Decode.string)
                decodeBounds
                (Decode.field "user" decodeUser)
                |> Decode.andThen
                    (\( type_, bounds, user ) ->
                        if type_ == "text_mention" then
                            Decode.succeed (TextMention bounds user)

                        else
                            Decode.fail
                                ("Expected field 'type' to be 'text_mention', but it was '"
                                    ++ type_
                                    ++ "'."
                                )
                    )
    in
    Decode.oneOf
        [ simple
        , textLink
        , textMention
        ]


decodeMessage : Decode.Decoder Message
decodeMessage =
    let
        decodeEntities =
            Decode.maybe (Decode.field "entities" (Decode.list decodeMessageEntity))
                |> Decode.map (Maybe.withDefault [])
    in
    Decode.map5
        Message
        (Decode.field "message_id" Decode.int |> Decode.map Id)
        (Decode.field "date" Decode.int)
        (Decode.field "chat" decodeChat)
        (Decode.field "text" Decode.string)
        decodeEntities


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
        (Decode.maybe <| Decode.field "last_name" Decode.string)
        (Decode.maybe <| Decode.field "username" Decode.string)
        (Decode.maybe <| Decode.field "language_code" Decode.string)


type Id a
    = Id Int


encodeId : Id a -> Encode.Value
encodeId (Id id) =
    Encode.int id



-- RESPONSE


type alias SendMessage =
    { chat_id : Id ChatTag
    , text : String
    , parse_mode : Maybe ParseMode
    }


type ParseMode
    = Markdown
    | Html


encodeSendMessage : SendMessage -> Encode.Value
encodeSendMessage sendMessage =
    let
        parse_mode =
            Maybe.map
                (\mode ->
                    case mode of
                        Markdown ->
                            Encode.string "Markdown"

                        Html ->
                            Encode.string "HTML"
                )
                sendMessage.parse_mode
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "chat_id", encodeId sendMessage.chat_id )
        , ( "text", Encode.string sendMessage.text )
        , ( "parse_mode", parse_mode )
        ]



-- TEST


makeTestId : Int -> Id a
makeTestId id =
    Id id
