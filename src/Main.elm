port module Main exposing (main)

import Elmegram
import Elmegram.Runner exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import RelevantXkcdBot as Bot
import Telegram


main =
    Platform.worker
        { init = init Bot.init
        , update = update
        , subscriptions = subscriptions incomingUpdatePort NewUpdate
        }


type alias Model =
    Bot.Model


{-| Simple type without custom types. This can be used by init, forcing errors
on the JS side instead of in a Decoder.

The only difference to `Telegram.User` is the id field. The real user has
a phantom type that guards against mixing incompatible ids.

-}
type alias RawUser =
    { id : Int
    , is_bot : Bool
    , first_name : String
    , last_name : Maybe String
    , username : Maybe String
    , language_code : Maybe String
    }


init : BotInit Model -> RawUser -> ( Model, Cmd Msg )
init botInit rawBot =
    let
        bot =
            -- Small hack to make type safe ID.
            { id = Telegram.makeTestId rawBot.id
            , is_bot = rawBot.is_bot
            , first_name = rawBot.first_name
            , last_name = rawBot.last_name
            , username = rawBot.username
            , language_code = rawBot.language_code
            }
    in
    ( botInit bot, Cmd.none )


type Msg
    = NewUpdate UpdateResult
    | BotMsg Bot.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUpdate result ->
            processUpdate handleUpdate result model errorPort

        BotMsg botMsg ->
            Bot.update botMsg model
                |> updateFromResponse


type alias UpdateResult =
    Result Decode.Error Telegram.Update


type alias ErrorPort msg =
    String -> Cmd msg


type alias UpdateHandler msg model =
    Telegram.Update -> model -> ( model, Cmd msg )


processUpdate : UpdateHandler msg model -> UpdateResult -> model -> ErrorPort msg -> ( model, Cmd msg )
processUpdate updateHandler result model error =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> error )

        Ok newUpdate ->
            updateHandler newUpdate model


handleUpdate : Telegram.Update -> Model -> ( Model, Cmd Msg )
handleUpdate newUpdate model =
    Bot.handle newUpdate model
        |> updateFromResponse


updateFromResponse : Elmegram.Response Bot.Model Bot.Msg -> ( Model, Cmd Msg )
updateFromResponse response =
    ( response.model, cmdFromResponse response )


cmdFromResponse : Elmegram.Response Bot.Model Bot.Msg -> Cmd Msg
cmdFromResponse response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ (if List.isEmpty response.methods then
                    []

                else
                    [ methodPort (Encode.list Elmegram.encodeMethod response.methods) ]
               )
        )



-- PORTS


port errorPort : String -> Cmd msg


port methodPort : Encode.Value -> Cmd msg


port incomingUpdatePort : (Encode.Value -> msg) -> Sub msg
