port module Main exposing (main)

import Elmergram
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


{-| Type that can be used by init, thus forcing errors on the JS side.

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
    = NewUpdate Elmergram.UpdateResult
    | BotMsg Bot.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUpdate result ->
            Elmergram.processUpdate error handleUpdate result model

        BotMsg botMsg ->
            Bot.update botMsg model.bot
                |> updateFromResponse


handleUpdate : Telegram.Update -> Model -> ( Model, Cmd Msg )
handleUpdate newUpdate model =
    Bot.handle newUpdate model.bot
        |> updateFromResponse


updateFromResponse : Bot.Response -> ( Model, Cmd Msg )
updateFromResponse response =
    ( { bot = response.model }, cmdFromResponse response )


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
