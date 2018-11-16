port module Main exposing (main)

import Platform


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init =
    Debug.todo ""


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Debug.todo ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.todo ""
