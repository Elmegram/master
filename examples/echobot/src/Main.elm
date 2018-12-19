port module Main exposing (bot, main)

import Elmegram
import Elmegram.Runner exposing (Method(..), Response)
import Json.Encode as Encode
import Telegram


main =
    Elmegram.Runner.botRunner
        bot
        { incomingUpdatePort = incomingUpdatePort
        , errorPort = errorPort
        }



-- PORTS


port incomingUpdatePort : (Encode.Value -> msg) -> Sub msg


port errorPort : String -> Cmd msg



-- BOT


bot =
    { init = init
    , newUpdateMsg = newUpdateMsg
    , update = update
    }


type alias Model =
    ()


init : Telegram.User -> Model
init _ =
    ()


type Msg
    = NewUpdate Telegram.Update


newUpdateMsg =
    NewUpdate


type alias Res =
    Response Model Msg


update : Msg -> Model -> Res
update msg model =
    case msg of
        NewUpdate newUpdate ->
            case newUpdate.content of
                Telegram.MessageUpdate message ->
                    Response
                        [ Elmegram.makeAnswer message.chat ("You said: " ++ message.text) |> SendMessageMethod ]
                        model
                        Cmd.none

                _ ->
                    Response [] model Cmd.none
