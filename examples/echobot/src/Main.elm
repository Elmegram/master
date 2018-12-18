port module Main exposing (bot, main)

import Elmegram
import Elmegram.Runner
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


type alias Response =
    Elmegram.Response Model Msg


update : Msg -> Model -> Response
update msg model =
    case msg of
        NewUpdate newUpdate ->
            case newUpdate.content of
                Telegram.MessageUpdate message ->
                    Elmegram.Response
                        [ Elmegram.answer message.chat ("You said: " ++ message.text) ]
                        model
                        Cmd.none

                _ ->
                    Elmegram.Response [] model Cmd.none
