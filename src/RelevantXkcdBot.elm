module RelevantXkcdBot exposing (Model, Msg, handle, init, update)

import Elmegram
import Http
import Json.Decode as Decode
import RelevantXkcd
import Telegram
import Url


type alias Response =
    Elmegram.Response Model Msg


type alias Model =
    { self : Telegram.User }


init : Telegram.User -> Model
init user =
    { self = user }


handle : Telegram.Update -> Model -> Response
handle newUpdate model =
    case newUpdate.content of
        Telegram.MessageUpdate message ->
            if Elmegram.matchesCommand "start" message || Elmegram.matchesCommand "help" message then
                simply [ helpMessage model.self message.chat ] model

            else if Elmegram.containsCommand message then
                simply [ commandNotFoundMessage model.self message ] model

            else
                let
                    getXkcd =
                        RelevantXkcd.fetch
                            message.text
                            (\result ->
                                case result of
                                    Ok xkcds ->
                                        case xkcds of
                                            bestMatch :: _ ->
                                                bestMatch.previewUrl
                                                    |> Url.toString
                                                    |> SendMessage message.chat

                                            _ ->
                                                SendMessage message.chat "No relevant xkcd found."

                                    Err _ ->
                                        Fail message.chat
                            )
                in
                do [] model getXkcd

        Telegram.InlineQueryUpdate inlineQuery ->
            let
                getXkcd =
                    RelevantXkcd.fetch
                        inlineQuery.query
                        (\result ->
                            case result of
                                Ok xkcds ->
                                    AnswerQuery inlineQuery xkcds

                                Err _ ->
                                    NoOp
                        )
            in
            do [] model getXkcd


type Msg
    = NoOp
    | SendMessage Telegram.Chat String
    | AnswerQuery Telegram.InlineQuery (List RelevantXkcd.Xkcd)
    | Fail Telegram.Chat


update : Msg -> Model -> Response
update msg model =
    case msg of
        NoOp ->
            keep model

        SendMessage chat text ->
            simply [ Elmegram.answer chat text ] model

        AnswerQuery id xkcds ->
            let
                results =
                    List.indexedMap
                        (\index xkcd ->
                            Elmegram.makeInlineQueryResultArticleText
                                (String.fromInt xkcd.id)
                                ("xckd #" ++ String.fromInt index)
                                (Url.toString xkcd.previewUrl)
                        )
                        xkcds
            in
            simply [ Elmegram.answerInlineQuery id results ] model

        Fail chat ->
            simply [ Elmegram.answer chat "Sorry, I had a problem finding a joke..." ] model


helpMessage : Telegram.User -> Telegram.Chat -> Elmegram.Method
helpMessage self chat =
    Elmegram.answerFormatted
        chat
        (Elmegram.format
            Telegram.Markdown
            (helpText self)
        )


helpText : Telegram.User -> String
helpText self =
    "Type `@"
        ++ Elmegram.getDisplayName self
        ++ " <query>` in any chat to search for [relevant xkcd](https://relevantxkcd.appspot.com/) comics."
        ++ "To get the latest comics, just enter nothing as the query.\n"
        ++ "\n"
        ++ "You can also just send me messages here. I will answer with the xkcd most relevant to what you sent me."


commandNotFoundMessage : Telegram.User -> Telegram.TextMessage -> Elmegram.Method
commandNotFoundMessage self message =
    Elmegram.replyFormatted
        message
        (Elmegram.format
            Telegram.Markdown
            ("I did not understand that command.\n\n" ++ helpText self)
        )



-- HELPERS


do : List Elmegram.Method -> Model -> Cmd Msg -> Response
do methods model cmd =
    { methods = methods
    , model = model
    , command = cmd
    }


simply : List Elmegram.Method -> Model -> Response
simply methods model =
    { methods = methods
    , model = model
    , command = Cmd.none
    }


keep : Model -> Response
keep model =
    { methods = []
    , model = model
    , command = Cmd.none
    }
