module HttpClient exposing (Error(..), auth, errorToString, getWithAuth, try)

import Dict
import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)
import Url exposing (Url)


type Auth
    = Auth String


auth : String -> Auth
auth str =
    Auth str


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadRequest String
    | Unauthorized String
    | Forbidden String
    | NotFound String
    | RequestTimeout String
    | Conflict String
    | TooManyRequests String
    | OtherClientError Int String
    | InternalServerError String
    | BadGateway String
    | ServiceUnavailable String
    | OtherServerError Int String
    | BadBody String


getWithAuth : Url -> Auth -> Decoder a -> Task Error a
getWithAuth url auth_ decoder =
    taskWithAuth "GET" url auth_ decoder


taskWithAuth : String -> Url -> Auth -> Decoder a -> Task Error a
taskWithAuth method url (Auth auth_) decoder =
    Http.task
        { method = method
        , headers =
            [ Http.header "authorization" auth_
            , Http.header "accept" "applicaiton/json"
            ]
        , url = Url.toString url
        , body = Http.emptyBody
        , resolver = Http.stringResolver (resolveStringResponse decoder)
        , timeout = Just 60000
        }


resolveStringResponse : Decoder a -> Http.Response String -> Result Error a
resolveStringResponse decoder res =
    case res of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                400 ->
                    Err (BadRequest body)

                401 ->
                    Err (Unauthorized body)

                403 ->
                    Err (Forbidden body)

                404 ->
                    Err (NotFound body)

                408 ->
                    Err (RequestTimeout body)

                409 ->
                    Err (Conflict body)

                429 ->
                    Err (TooManyRequests body)

                500 ->
                    Err (InternalServerError body)

                502 ->
                    Err (BadGateway body)

                503 ->
                    Err (ServiceUnavailable body)

                other ->
                    if other < 500 then
                        Err (OtherClientError other body)

                    else
                        Err (OtherServerError other body)

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok a ->
                    Ok a

                Err e ->
                    Err (BadBody (Json.Decode.errorToString e))


try : (a -> msg) -> (Error -> msg) -> Task Error a -> Cmd msg
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


errorToString : Error -> String
errorToString e =
    (++) "[HTTP] " <|
        case e of
            BadUrl badUrl ->
                "Bad URL: " ++ badUrl

            Timeout ->
                "Timeout"

            NetworkError ->
                "NetworkError"

            BadRequest body ->
                "BadRequest: " ++ body

            Unauthorized body ->
                "Unauthorized: " ++ body

            Forbidden body ->
                "Forbidden: " ++ body

            NotFound body ->
                "NotFound: " ++ body

            RequestTimeout body ->
                "RequestTimeout: " ++ body

            Conflict body ->
                "Conflict: " ++ body

            TooManyRequests body ->
                "TooManyRequests: " ++ body

            OtherClientError code body ->
                "OtherClientError (" ++ String.fromInt code ++ "): " ++ body

            InternalServerError body ->
                "InternalServerError: " ++ body

            BadGateway body ->
                "BadGateway: " ++ body

            ServiceUnavailable body ->
                "ServiceUnavailable: " ++ body

            OtherServerError code body ->
                "OtherServerError (" ++ String.fromInt code ++ "): " ++ body

            BadBody errString ->
                "Bad body: " ++ errString
