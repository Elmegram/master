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
                        RelevantXkcd.fetchIds
                            message.text
                            (\result ->
                                case result of
                                    Ok ids ->
                                        case ids of
                                            bestMatch :: _ ->
                                                FetchXkcd
                                                    (\res ->
                                                        case res of
                                                            Ok xkcd ->
                                                                SendXkcdMessage message.chat xkcd

                                                            Err err ->
                                                                SendMessage message.chat "Error getting xkcds."
                                                    )
                                                    bestMatch

                                            _ ->
                                                SendMessage message.chat "No relevant xkcd found."

                                    Err _ ->
                                        SendMessage message.chat "Error getting xkcds."
                            )
                in
                do [] model getXkcd

        Telegram.InlineQueryUpdate inlineQuery ->
            let
                getXkcd =
                    RelevantXkcd.fetchIds
                        inlineQuery.query
                        (\result ->
                            case result of
                                Ok ids ->
                                    FetchXkcds
                                        (\res ->
                                            case res of
                                                Ok xkcds ->
                                                    AnswerQuery inlineQuery xkcds

                                                Err err ->
                                                    AnswerQuery inlineQuery []
                                        )
                                        ids

                                Err _ ->
                                    NoOp
                        )
            in
            do [] model getXkcd


type Msg
    = NoOp
    | SendMessage Telegram.Chat String
    | AnswerQuery Telegram.InlineQuery (List RelevantXkcd.Xkcd)
    | FetchXkcd (Result String RelevantXkcd.Xkcd -> Msg) RelevantXkcd.XkcdId
    | FetchXkcds (Result String (List RelevantXkcd.Xkcd) -> Msg) (List RelevantXkcd.XkcdId)
    | SendXkcdMessage Telegram.Chat RelevantXkcd.Xkcd


update : Msg -> Model -> Response
update msg model =
    case msg of
        NoOp ->
            keep model

        SendMessage to text ->
            simply [ Elmegram.answer to text ] model

        AnswerQuery to xkcds ->
            let
                results =
                    List.map
                        (\xkcd ->
                            Elmegram.inlineQueryResultArticle
                                { id = String.fromInt <| RelevantXkcd.getId xkcd
                                , title = RelevantXkcd.getTitle xkcd
                                , description = RelevantXkcd.getTranscript xkcd |> Maybe.withDefault "No description available."
                                , message = Elmegram.makeInputMessageFormatted <| xkcdText xkcd
                                }
                        )
                        xkcds
            in
            simply [ Elmegram.answerInlineQuery to results ] model

        FetchXkcd tag id ->
            do [] model (RelevantXkcd.fetchXkcd tag id)

        FetchXkcds tag ids ->
            do [] model (RelevantXkcd.fetchXkcds tag ids)

        SendXkcdMessage to xkcd ->
            simply [ Elmegram.answerFormatted to (xkcdText xkcd) ] model


xkcdText : RelevantXkcd.Xkcd -> Elmegram.FormattedText
xkcdText xkcd =
    Elmegram.format Telegram.Html
        (("<b>" ++ RelevantXkcd.getTitle xkcd ++ "</b>\n")
            ++ (Url.toString <| RelevantXkcd.getPreviewUrl xkcd)
        )


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
