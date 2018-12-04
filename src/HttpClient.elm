module HttpClient exposing
    ( Error(..), Req, Failure, auth, errorToString
    , try, getWithAuth, postFormWithAuth, postJsonWithAuth
    )

{-| Thin wrapper around Http.

@docs Error, Req, Failure, auth, errorToString
@docs try, getWithAuth, postFormWithAuth, postJsonWithAuth

-}

import Dict
import Http
import Json.Decode exposing (Decoder, Value)
import Task exposing (Task)
import Url exposing (Url)


type Auth
    = Auth String -- Bare API token carried in "authorization" header


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


type alias Req =
    { method : String
    , url : Url
    }


type alias Failure =
    ( Error, Req )


getWithAuth : Url -> Auth -> Decoder a -> Task Failure a
getWithAuth url auth_ decoder =
    taskWithAuth "GET" url Http.emptyBody auth_ decoder


{-| Send POST request with multipart/form-data.

Not using application/x-www-form-urlencoded, so:

  - it can send files/byte sequences
  - contents are (probably) not logged

-}
postFormWithAuth : Url -> List Http.Part -> Auth -> Decoder a -> Task Failure a
postFormWithAuth url parts auth_ decoder =
    taskWithAuth "POST" url (Http.multipartBody parts) auth_ decoder


postJsonWithAuth : Url -> Value -> Auth -> Decoder a -> Task Failure a
postJsonWithAuth url json auth_ decoder =
    taskWithAuth "POST" url (Http.jsonBody json) auth_ decoder


taskWithAuth : String -> Url -> Http.Body -> Auth -> Decoder a -> Task Failure a
taskWithAuth method url body (Auth auth_) decoder =
    Http.task
        { method = method
        , headers =
            [ Http.header "authorization" auth_
            , Http.header "accept" "applicaiton/json"
            ]
        , url = Url.toString url
        , body = body
        , resolver = Http.stringResolver (resolveStringResponse (Req method url) decoder)
        , timeout = Just 60000
        }


resolveStringResponse : Req -> Decoder a -> Http.Response String -> Result Failure a
resolveStringResponse req decoder res =
    case res of
        Http.BadUrl_ url ->
            Err ( BadUrl url, req )

        Http.Timeout_ ->
            Err ( Timeout, req )

        Http.NetworkError_ ->
            Err ( NetworkError, req )

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                400 ->
                    Err ( BadRequest body, req )

                401 ->
                    Err ( Unauthorized body, req )

                403 ->
                    Err ( Forbidden body, req )

                404 ->
                    Err ( NotFound body, req )

                408 ->
                    Err ( RequestTimeout body, req )

                409 ->
                    Err ( Conflict body, req )

                429 ->
                    Err ( TooManyRequests body, req )

                500 ->
                    Err ( InternalServerError body, req )

                502 ->
                    Err ( BadGateway body, req )

                503 ->
                    Err ( ServiceUnavailable body, req )

                other ->
                    if other < 500 then
                        Err ( OtherClientError other body, req )

                    else
                        Err ( OtherServerError other body, req )

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok a ->
                    Ok a

                Err e ->
                    Err ( BadBody (Json.Decode.errorToString e), req )


try : (a -> msg) -> (Failure -> msg) -> Task Failure a -> Cmd msg
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
