module RelevantXkcd exposing (fetch)

import Http
import Url exposing (Url)
import Url.Builder


type alias Xkcd =
    { id : Int
    , previewUrl : Url
    }


fetch : String -> (Result String Xkcd -> msg) -> Cmd msg
fetch query tag =
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
                                |> Result.andThen
                                    (\list ->
                                        case list of
                                            bestMatch :: _ ->
                                                Ok bestMatch

                                            _ ->
                                                Err "No relevant xkcd found."
                                    )
                        )
                    >> tag
                )
        , timeout = Nothing
        , tracker = Nothing
        }


parseResponse : String -> Result String (List Xkcd)
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


parseXkcd : String -> Result String Xkcd
parseXkcd line =
    case String.words line of
        idString :: urlString :: [] ->
            let
                parseId =
                    String.toInt

                parseUrl =
                    (++) "https://www.explainxkcd.com"
                        >> Url.fromString
            in
            case
                ( idString, urlString )
                    |> Tuple.mapBoth parseId parseUrl
            of
                ( Just id, Just url ) ->
                    Ok (Xkcd id url)

                _ ->
                    Err "Malformed line. Could not convert id or url."

        malformed ->
            Err <| "Malformed line. Expected 2, got " ++ (List.length malformed |> String.fromInt) ++ "."


queryUrl : String -> Url
queryUrl query =
    { protocol = Url.Https
    , host = "relevantxkcd.appspot.com"
    , port_ = Nothing
    , path = "/process"
    , query = Just ("action=xkcd&query=" ++ query)
    , fragment = Nothing
    }
