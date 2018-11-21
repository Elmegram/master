port module Main exposing (main)

import Elmegram
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import RelevantXkcdBot as Bot
import Telegram


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { bot : Bot.Model }


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


init : RawUser -> ( Model, Cmd Msg )
init rawBot =
    let
        bot =
            { id = Telegram.makeTestId rawBot.id
            , is_bot = rawBot.is_bot
            , first_name = rawBot.first_name
            , last_name = rawBot.last_name
            , username = rawBot.username
            , language_code = rawBot.language_code
            }
    in
    ( { bot = Bot.init bot }, Cmd.none )


type Msg
    = NewUpdate UpdateResult
    | BotMsg Bot.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUpdate result ->
            processUpdate errorPort handleUpdate result model

        BotMsg botMsg ->
            Bot.update botMsg model.bot
                |> updateFromResponse


handleUpdate : Telegram.Update -> Model -> ( Model, Cmd Msg )
handleUpdate newUpdate model =
    Bot.handle newUpdate model.bot
        |> updateFromResponse


updateFromResponse : Elmegram.Response Bot.Model Bot.Msg -> ( Model, Cmd Msg )
updateFromResponse response =
    ( { bot = response.model }, cmdFromResponse response )


cmdFromResponse : Elmegram.Response Bot.Model Bot.Msg -> Cmd Msg
cmdFromResponse response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ (Maybe.map (encodeSendMessage >> sendMessagePort >> List.singleton) response.message
                    |> Maybe.withDefault []
               )
        )


port errorPort : String -> Cmd msg


port sendMessagePort : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    incomingUpdatePort (decodeUpdate >> NewUpdate)


port incomingUpdatePort : (Encode.Value -> msg) -> Sub msg



-- I/O


decodeUpdate : Encode.Value -> Result Decode.Error Telegram.Update
decodeUpdate =
    Decode.decodeValue Telegram.decodeUpdate


encodeSendMessage : Telegram.SendMessage -> Encode.Value
encodeSendMessage =
    Telegram.encodeSendMessage


type alias UpdateResult =
    Result Decode.Error Telegram.Update


type alias ErrorPort msg =
    String -> Cmd msg


type alias UpdateHandler msg model =
    Telegram.Update -> model -> ( model, Cmd msg )


processUpdate : ErrorPort msg -> UpdateHandler msg model -> UpdateResult -> model -> ( model, Cmd msg )
processUpdate error updateHandler result model =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> error )

        Ok newUpdate ->
            updateHandler newUpdate model
