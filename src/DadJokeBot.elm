module DadJokeBot exposing (Model, Msg, Response, handle, init, update)

import Elmergram
import Http
import Json.Decode as Decode
import Telegram
import Url


type alias Model =
    { self : Telegram.User }


init : Telegram.User -> Model
init user =
    { self = user }


type alias Response =
    { message : Maybe Telegram.SendMessage
    , model : Model
    , command : Cmd Msg
    }


handle : Telegram.Update -> Model -> Response
handle newUpdate model =
    case newUpdate.content of
        Telegram.MessageUpdate message ->
            if String.contains "start" message.text || String.contains "help" message.text then
                simply (helpMessage model.self message.chat) model

            else
                let
                    getJoke =
                        Http.request
                            { method = "GET"
                            , headers = [ Http.header "Accept" "application/json" ]
                            , url = "https://icanhazdadjoke.com/"
                            , body = Http.emptyBody
                            , expect =
                                Http.expectJson
                                    (\result ->
                                        case result of
                                            Ok joke ->
                                                NewJoke message.chat joke

                                            Err reason ->
                                                Fail message.chat
                                    )
                                    (Decode.field "joke" Decode.string)
                            , timeout = Nothing
                            , tracker = Nothing
                            }
                in
                do Nothing model getJoke


type Msg
    = NewJoke Telegram.Chat String
    | Fail Telegram.Chat


update : Msg -> Model -> Response
update msg model =
    case msg of
        Fail chat ->
            simply (Elmergram.answer chat "Sorry, I had a problem finding a joke...") model

        NewJoke chat joke ->
            simply (Elmergram.answer chat joke) model


helpMessage : Telegram.User -> Telegram.Chat -> Telegram.SendMessage
helpMessage self chat =
    Elmergram.answerFormatted
        chat
        (Elmergram.format
            Telegram.Markdown
            ("Type `@"
                ++ Elmergram.getName self
                ++ " <query>` in any chat to search for [relevant xkcd](https://relevantxkcd.appspot.com/) comics."
                ++ "When the query is empty, the latest XKCD comics are sent."
            )
        )



-- HELPERS


do : Maybe Telegram.SendMessage -> Model -> Cmd Msg -> Response
do message model cmd =
    { message = message
    , model = model
    , command = cmd
    }


simply : Telegram.SendMessage -> Model -> Response
simply message model =
    { message = Just message
    , model = model
    , command = Cmd.none
    }


keep : Model -> Response
keep model =
    { message = Nothing
    , model = model
    , command = Cmd.none
    }
