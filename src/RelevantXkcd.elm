module RelevantXkcd exposing
    ( Xkcd
    , XkcdId
    , fetchIds
    , fetchXkcd
    , fetchXkcds
    , getExplainUrl
    , getId
    , getMouseOver
    , getPreviewUrl
    , getTitle
    , getTranscript
    )

import Http
import Json.Decode as Decode
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder



-- XKCD


type Xkcd
    = Xkcd XkcdContent


type alias XkcdContent =
    { id : Int
    , previewUrl : Url
    , title : String
    , mouseOver : String
    , transcript : Maybe String
    }


getId : Xkcd -> Int
getId (Xkcd xkcd) =
    xkcd.id


getPreviewUrl : Xkcd -> Url
getPreviewUrl (Xkcd xkcd) =
    xkcd.previewUrl


getTitle : Xkcd -> String
getTitle (Xkcd xkcd) =
    xkcd.title


getMouseOver : Xkcd -> String
getMouseOver (Xkcd xkcd) =
    xkcd.mouseOver


getTranscript : Xkcd -> Maybe String
getTranscript (Xkcd xkcd) =
    xkcd.transcript


getExplainUrl : Xkcd -> Url
getExplainUrl (Xkcd xkcd) =
    { protocol = Url.Https
    , host = "www.explainxkcd.com"
    , port_ = Nothing
    , path = "/wiki/index.php/" ++ String.fromInt xkcd.id
    , query = Nothing
    , fragment = Nothing
    }



-- METHODS


fetchIds : String -> (Result String (List XkcdId) -> msg) -> Cmd msg
fetchIds query tag =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.toString (queryUrl query)
        , body = Http.emptyBody
        , expect =
            Http.expectString
                (Result.mapError
                    (\err ->
                        "Error fetching xkcd."
                    )
                    >> Result.andThen
                        (\body ->
                            parseResponse body
                        )
                    >> tag
                )
        , timeout = Nothing
        , tracker = Nothing
        }


parseResponse : String -> Result String (List XkcdId)
parseResponse body =
    let
        dropFromEnd amount list =
            List.take (List.length list - amount) list
    in
    String.lines body
        -- The first two entries are statistics.
        |> List.drop 2
        |> dropFromEnd 1
        |> List.map parseXkcd
        |> List.foldl
            (\result list ->
                Result.map2 (\xkcd existing -> existing ++ [ xkcd ]) result list
            )
            (Ok [])


type alias XkcdId =
    Int


parseXkcd : String -> Result String XkcdId
parseXkcd line =
    case String.words line of
        idString :: urlString :: [] ->
            case String.toInt idString of
                Just id ->
                    Ok id

                _ ->
                    Err "Malformed line. Could not convert id."

        malformed ->
            Err <| "Malformed line. Expected 2 fields, got " ++ (List.length malformed |> String.fromInt) ++ "."


queryUrl : String -> Url
queryUrl query =
    { protocol = Url.Https
    , host = "relevantxkcd.appspot.com"
    , port_ = Nothing
    , path = "/process"
    , query = Just ("action=xkcd&query=" ++ query)
    , fragment = Nothing
    }


fetchXkcd : (Result String Xkcd -> msg) -> XkcdId -> Cmd msg
fetchXkcd tag id =
    fetchXkcdTask id
        |> Task.attempt tag


fetchXkcds : (Result String (List Xkcd) -> msg) -> List XkcdId -> Cmd msg
fetchXkcds tag ids =
    Task.sequence (List.map fetchXkcdTask ids)
        |> Task.attempt tag


fetchXkcdTask : XkcdId -> Task String Xkcd
fetchXkcdTask id =
    Http.task
        { method = "GET"
        , headers = []
        , url = Url.toString (xkcdInfoUrl id)
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.GoodStatus_ _ res ->
                            let
                                decodeUrl =
                                    Decode.string
                                        |> Decode.andThen
                                            (\urlString ->
                                                case Url.fromString urlString of
                                                    Just url ->
                                                        Decode.succeed url

                                                    Nothing ->
                                                        Decode.fail ("Invalid url '" ++ urlString ++ "'.")
                                            )

                                decodeTranscript =
                                    Decode.string
                                        |> Decode.map
                                            (\transcript ->
                                                if String.isEmpty transcript then
                                                    Nothing

                                                else
                                                    Just transcript
                                            )
                            in
                            Decode.decodeString
                                (Decode.map4
                                    (XkcdContent id)
                                    (Decode.field "img" decodeUrl)
                                    (Decode.field "title" Decode.string)
                                    (Decode.field "alt" Decode.string)
                                    (Decode.field "transcript" decodeTranscript)
                                )
                                res
                                |> Result.map Xkcd
                                |> Result.mapError Decode.errorToString

                        _ ->
                            Err "Error fetching xkcd."
                )
        , timeout = Nothing
        }


xkcdInfoUrl : XkcdId -> Url
xkcdInfoUrl id =
    { protocol = Url.Https
    , host = "xkcd.com"
    , port_ = Nothing
    , path = "/" ++ String.fromInt id ++ "/info.0.json"
    , query = Nothing
    , fragment = Nothing
    }
