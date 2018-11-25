module Elmegram.Runner exposing (BotHandle, BotInit, BotUpdate, ErrorPort, IncomingUpdatePort, MethodPort, subscriptions)

import Elmegram exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram


type alias BotInit model =
    Telegram.User -> model


type alias BotHandle model msg =
    Telegram.Update -> model -> Response model msg


type alias BotUpdate model msg =
    msg -> model -> Response model msg


type alias IncomingUpdatePort msg =
    (Encode.Value -> msg) -> Sub msg


type alias MethodPort msg =
    Encode.Value -> Cmd msg


type alias ErrorPort msg =
    String -> Cmd msg


subscriptions : IncomingUpdatePort msg -> (Result Decode.Error Telegram.Update -> msg) -> model -> Sub msg
subscriptions incomingUpdatePort tag model =
    incomingUpdatePort (Decode.decodeValue Telegram.decodeUpdate >> tag)
