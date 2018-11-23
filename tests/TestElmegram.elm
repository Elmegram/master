module TestElmegram exposing (suite)

import Elmegram
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Telegram.Test as TeleTest
import Test exposing (..)


suite : Test
suite =
    describe "Elmegram"
        [ describe "getDisplayName"
            [ test "gives username, if all are specified" <|
                \_ ->
                    let
                        user =
                            TeleTest.makeUser
                    in
                    { user
                        | first_name = "Say my"
                        , last_name = Just "Username"
                        , username = Just "Heisenberg"
                    }
                        |> Elmegram.getDisplayName
                        |> Expect.equal "Heisenberg"
            , test "gives first and lastname, if no username" <|
                \_ ->
                    let
                        user =
                            TeleTest.makeUser
                    in
                    { user
                        | first_name = "Display"
                        , last_name = Just "Me"
                        , username = Nothing
                    }
                        |> Elmegram.getDisplayName
                        |> Expect.equal "Display Me"
            , test "gives first name, if no other name" <|
                \_ ->
                    let
                        user =
                            TeleTest.makeUser
                    in
                    { user
                        | first_name = "Shorty"
                        , last_name = Nothing
                        , username = Nothing
                    }
                        |> Elmegram.getDisplayName
                        |> Expect.equal "Shorty"
            ]
        ]
