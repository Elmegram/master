module TestTelegram exposing (suite)

import Expect
import Fuzz exposing (..)
import Json.Decode as Decode
import Telegram
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "Telegram"
        [ describe "decode User"
            [ test "valid full User" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeUser
                        """
                        {
                            "id": 123,
                            "is_bot": false,
                            "first_name": "Kevin",
                            "last_name": "Spacey",
                            "username": "John Doe",
                            "language_code": "en"
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (Telegram.User
                                    (Telegram.makeTestId 123)
                                    False
                                    "Kevin"
                                    (Just "Spacey")
                                    (Just "John Doe")
                                    (Just "en")
                                )
                            )
            , test "minimal User" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeUser
                        """
                        {
                            "id": 59234,
                            "is_bot": false,
                            "first_name": "Minimalist"
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (Telegram.User
                                    (Telegram.makeTestId 59234)
                                    False
                                    "Minimalist"
                                    Nothing
                                    Nothing
                                    Nothing
                                )
                            )
            ]
        , describe "decode Chat"
            [ describe "valid chat type"
                (List.map
                    (\( field, constructor ) ->
                        test field <|
                            \_ ->
                                Decode.decodeString
                                    Telegram.decodeChat
                                    ("""
                                        {
                                            "id": 92533,
                                    """
                                        ++ ("\"type\": \"" ++ field ++ "\"")
                                        ++ """
                                        }
                                        """
                                    )
                                    |> Expect.equal
                                        (Ok
                                            (Telegram.Chat
                                                (Telegram.makeTestId 92533)
                                                constructor
                                            )
                                        )
                    )
                    [ ( "private", Telegram.Private )
                    , ( "group", Telegram.Group )
                    , ( "supergroup", Telegram.Supergroup )
                    , ( "channel", Telegram.Channel )
                    ]
                )
            , test "invalid chat type" <|
                \_ ->
                    case
                        Decode.decodeString
                            Telegram.decodeChat
                            """
                                {
                                    "id": 286,
                                    "type": "i am not a valid type"
                                }
                            """
                    of
                        Err error ->
                            let
                                message =
                                    Decode.errorToString error
                            in
                            if String.contains "i am not a valid type" message then
                                Expect.pass

                            else
                                Expect.fail
                                    ("Expected Err with error message mentioning actual value, but it was '"
                                        ++ message
                                        ++ "'."
                                    )

                        _ ->
                            Expect.fail "Expected Err, but got Ok."
            ]
        , describe "Message"
            [ describe "MessageEntity"
                [ describe "Bounds"
                    [ fuzz2 int int "valid bounds" <|
                        \offset length ->
                            Decode.decodeString
                                Telegram.decodeBounds
                                ("""
                                {
                                    """
                                    ++ ("\"offset\": " ++ String.fromInt offset ++ ",\n")
                                    ++ ("\"length\": " ++ String.fromInt length ++ "\n")
                                    ++ """
                                }
                                """
                                )
                                |> Expect.equal (Ok <| Telegram.Bounds offset length)
                    ]
                , describe "valid simple types"
                    (List.map
                        (\( field, constructor ) ->
                            test field <|
                                \_ ->
                                    Decode.decodeString
                                        Telegram.decodeMessageEntity
                                        ("""
                                        {
                                        """
                                            ++ ("\"type\": \"" ++ field ++ "\",\n")
                                            ++ """
                                            "offset": 0,
                                            "length": 3
                                        }
                                        """
                                        )
                                        |> Expect.equal
                                            (Ok <|
                                                constructor <|
                                                    Telegram.Bounds 0 3
                                            )
                        )
                        [ ( "mention", Telegram.Mention )
                        , ( "hashtag", Telegram.Hashtag )
                        , ( "cashtag", Telegram.Cashtag )
                        , ( "bot_command", Telegram.BotCommand )
                        , ( "url", Telegram.Url )
                        , ( "email", Telegram.Email )
                        , ( "phone_number", Telegram.PhoneNumber )
                        , ( "bold", Telegram.Bold )
                        , ( "italic", Telegram.Italic )
                        , ( "code", Telegram.Code )
                        , ( "pre", Telegram.Pre )
                        ]
                    )
                , describe "TextLink"
                    [ test "valid full" <|
                        \_ ->
                            Decode.decodeString
                                Telegram.decodeMessageEntity
                                """
                            {
                                "type": "text_link",
                                "offset": 49,
                                "length": 13,
                                "url": "https://elm-lang.org/"
                            }
                            """
                                |> Expect.equal
                                    (Ok <|
                                        Telegram.TextLink
                                            (Telegram.Bounds 49 13)
                                            (Url Url.Https "elm-lang.org" Nothing "/" Nothing Nothing)
                                    )
                    ]
                ]
            ]
        ]
