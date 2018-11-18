module DadJokeBot exposing (Model, Msg, Response, handle, initModel, update)

import Elmergram
import Http
import Json.Decode as Decode
import Telegram
import Url


type alias Model =
    {}


initModel : Model
initModel =
    {}


type alias Response =
    { message : Maybe Telegram.SendMessage
    , model : Model
    , command : Cmd Msg
    }


handle : Telegram.Update -> Model -> Response
handle newUpdate model =
    case newUpdate.content of
        Telegram.MessageUpdate message ->
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
