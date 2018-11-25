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
        , update = update Bot.handle Bot.update errorPort
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


update : BotHandle Bot.Model Bot.Msg -> BotUpdate Bot.Model Bot.Msg -> ErrorPort Msg -> Msg -> Model -> ( Model, Cmd Msg )
update botHandle botUpdate error msg model =
    case msg of
        NewUpdate result ->
            processUpdate botHandle result model error

        BotMsg botMsg ->
            botUpdate botMsg model
                |> updateFromResponse


type alias UpdateResult =
    Result Decode.Error Telegram.Update


processUpdate : BotHandle Bot.Model Bot.Msg -> UpdateResult -> Bot.Model -> ErrorPort Msg -> ( Bot.Model, Cmd Msg )
processUpdate updateHandler result model error =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> error )

        Ok newUpdate ->
            updateHandler newUpdate model
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
