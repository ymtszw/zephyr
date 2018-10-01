module HttpExtra exposing (auth, getWithAuth, try)

import Http
import Json.Decode exposing (Decoder)
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
