module RelevantXkcdBot exposing (Model, Msg, Response, handle, init, update)

import Elmergram
import Http
import Json.Decode as Decode
import RelevantXkcd
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
                    getXkcd =
                        RelevantXkcd.fetch
                            message.text
                            (\result ->
                                case result of
                                    Ok xkcd ->
                                        xkcd.previewUrl
                                            |> Url.toString
                                            |> SendMessage message.chat

                                    Err _ ->
                                        Fail message.chat
                            )
                in
                do Nothing model getXkcd


type Msg
    = SendMessage Telegram.Chat String
    | Fail Telegram.Chat


update : Msg -> Model -> Response
update msg model =
    case msg of
        Fail chat ->
            simply (Elmergram.answer chat "Sorry, I had a problem finding a joke...") model

        SendMessage chat text ->
            simply (Elmergram.answer chat text) model


helpMessage : Telegram.User -> Telegram.Chat -> Telegram.SendMessage
helpMessage self chat =
    Elmergram.answerFormatted
        chat
        (Elmergram.format
            Telegram.Markdown
            ("Type `@"
                ++ Elmergram.getName self
                ++ " <query>` in any chat to search for [relevant xkcd](https://relevantxkcd.appspot.com/) comics."
                ++ "To get the latest comics, just enter nothing as the query.\n\n"
                ++ "You can also just send me messages here. I will answer with the xkcd most relevant to what you sent me."
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
