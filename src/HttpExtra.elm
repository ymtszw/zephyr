module HttpExtra exposing (auth, errorToString, getWithAuth, try)

import Dict
import Http
import Json.Decode exposing (Decoder, errorToString)
import Task exposing (Task)
import Url exposing (Url)


type Auth
    = Auth String


auth : String -> Auth
auth str =
    Auth str


getWithAuth : Url -> Auth -> Decoder a -> Task Http.Error a
getWithAuth url auth_ decoder =
    reqWithAuth "GET" url auth_ decoder
        |> Http.toTask


reqWithAuth : String -> Url -> Auth -> Decoder a -> Http.Request a
reqWithAuth method url (Auth auth_) decoder =
    Http.request
        { method = method
        , headers =
            [ Http.header "authorization" auth_
            , Http.header "accept" "applicaiton/json"
            ]
        , url = Url.toString url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Just 60000
        , withCredentials = False
        }


try : (a -> msg) -> (Http.Error -> msg) -> Task Http.Error a -> Cmd msg
try fromOk fromErr task =
    let
        fromRes res =
            case res of
                Ok val ->
                    fromOk val

                Err httpError ->
                    fromErr httpError
    in
    Task.attempt fromRes task


errorToString : Http.Error -> String
errorToString e =
    (++) "[HTTP] " <|
        case e of
            Http.BadUrl badUrl ->
                "Bad URL: " ++ badUrl

            Http.Timeout ->
                "Timeout"

            Http.NetworkError ->
                "Network Error"

            Http.BadStatus r ->
                ("Bad status: " ++ statusToString r.status) :: responseToString r |> String.join "\n"

            Http.BadPayload decodeErr r ->
                ("Bad payload: " ++ decodeErr) :: responseToString r |> String.join "\n"


responseToString : Http.Response String -> List String
responseToString r =
    [ "URL: " ++ r.url
    , "Code: " ++ statusToString r.status
    , "Headers:"
    , r.headers |> Dict.toList |> List.map (\( k, v ) -> "  " ++ k ++ " : " ++ v) |> String.join "\n"
    , "Body:"
    , r.body
    ]


statusToString : { code : Int, message : String } -> String
statusToString status =
    String.fromInt status.code ++ " " ++ status.message
