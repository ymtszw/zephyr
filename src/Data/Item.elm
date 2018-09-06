module Data.Item exposing (Item, Media(..), decoder, encoder, textOnly, welcome)

import Element.Font
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url


type alias Item =
    { message : String
    , mediaMaybe : Maybe Media
    }


textOnly : String -> Item
textOnly message =
    Item message Nothing


type Media
    = Image Url.Url
    | Movie Url.Url


decoder : Decoder Item
decoder =
    D.map2 Item
        (D.field "message" D.string)
        (D.field "media" (D.maybe mediaDecoder))


mediaDecoder : Decoder Media
mediaDecoder =
    D.string
        |> D.andThen
            (\str ->
                case Url.fromString (String.dropLeft 5 str) of
                    Just url ->
                        if String.startsWith "IMAGE" str then
                            D.succeed (Image url)

                        else if String.startsWith "MOVIE" str then
                            D.succeed (Movie url)

                        else
                            D.fail ("Media URL is serialized incorrectly: " ++ str)

                    Nothing ->
                        D.fail ("Invalid media URL: " ++ str)
            )


encoder : Item -> E.Value
encoder { message, mediaMaybe } =
    E.object
        [ ( "message", E.string message )
        , ( "media", mediaMaybe |> Maybe.map mediaEncoder |> Maybe.withDefault E.null )
        ]


mediaEncoder : Media -> E.Value
mediaEncoder media =
    case media of
        Image url ->
            E.string ("IMAGE" ++ Url.toString url)

        Movie url ->
            E.string ("MOVIE" ++ Url.toString url)


welcome : Item
welcome =
    { message = "Welcome to Zephyr app!\n\nYou can add data producers and set up conditional feeds!"
    , mediaMaybe =
        Just <|
            Image
                { protocol = Url.Https
                , host = "cdn.dribbble.com"
                , port_ = Nothing
                , path = "/users/27231/screenshots/2432051/welcome.gif"
                , fragment = Nothing
                , query = Nothing
                }
    }
