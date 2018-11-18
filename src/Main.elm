port module Main exposing (main)

import DadJokeBot as Bot
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
    = NewUpdate Elmergram.UpdateResult
    | BotMsg Bot.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUpdate result ->
            Elmergram.processUpdate error handleUpdate result model

        BotMsg botMsg ->
            Bot.update botMsg model
                |> updateFromResponse


handleUpdate : Telegram.Update -> Model -> ( Model, Cmd Msg )
handleUpdate newUpdate model =
    Bot.handle newUpdate model
        |> updateFromResponse


updateFromResponse : Bot.Response -> ( Model, Cmd Msg )
updateFromResponse response =
    ( response.model, cmdFromResponse response )


cmdFromResponse : Bot.Response -> Cmd Msg
cmdFromResponse response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ (Maybe.map (Elmergram.encodeSendMessage >> sendMessage >> List.singleton) response.message
                    |> Maybe.withDefault []
               )
        )


port error : String -> Cmd msg


port sendMessage : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    incomingUpdate (Elmergram.decodeUpdate >> NewUpdate)


port incomingUpdate : (Encode.Value -> msg) -> Sub msg
