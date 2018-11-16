port module Main exposing (main)

import Elmergram
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Telegram


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {}


initModel =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


type Msg
    = NewUpdate (Result Decode.Error Telegram.Update)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUpdate result ->
            case result of
                Err err ->
                    ( model, Decode.errorToString err |> error )

                Ok newUpdate ->
                    handleUpdate model newUpdate


handleUpdate : Model -> Telegram.Update -> ( Model, Cmd Msg )
handleUpdate model newUpdate =
    let
        answer =
            case newUpdate.content of
                Telegram.MessageUpdate message ->
                    if String.contains "hi" (String.toLower message.text) then
                        Elmergram.answer "Hi!" message.chat

                    else if String.contains "bye" (String.toLower message.text) then
                        Elmergram.answer "Bye!" message.chat

                    else
                        Elmergram.answer message.text message.chat
    in
    ( model, Elmergram.encodeSendMessage answer |> sendMessage )


port error : String -> Cmd msg


port sendMessage : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    incomingUpdate (Elmergram.decodeUpdate >> NewUpdate)


port incomingUpdate : (Encode.Value -> msg) -> Sub msg
