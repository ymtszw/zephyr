module Data.Item exposing (Item(..), Media(..), decoder, encode, initBroker, textOnly, welcome)

import Broker exposing (Broker)
import Data.Producer.Discord as Discord
import Element.Font
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Url


type Item
    = DiscordItem Discord.Message
    | SystemItem
        { message : String
        , mediaMaybe : Maybe Media
        }


textOnly : String -> Item
textOnly message =
    SystemItem { message = message, mediaMaybe = Nothing }


type Media
    = Image Url.Url
    | Movie Url.Url


encode : Item -> E.Value
encode item =
    case item of
        DiscordItem discordMessage ->
            E.tagged "DiscordItem" (Discord.encodeMessage discordMessage)

        SystemItem { message, mediaMaybe } ->
            E.tagged "SystemItem" <|
                E.object
                    [ ( "message", E.string message )
                    , ( "media", E.maybe encodeMedia mediaMaybe )
                    ]


encodeMedia : Media -> E.Value
encodeMedia media =
    case media of
        Image url ->
            E.tagged "Image" (E.string (Url.toString url))

        Movie url ->
            E.tagged "Movie" (E.string (Url.toString url))


decoder : Decoder Item
decoder =
    D.oneOf
        [ D.tagged "DiscordItem" DiscordItem Discord.messageDecoder
        , D.tagged "SystemItem" SystemItem systemMessageDecoder

        -- Old version
        , D.map SystemItem systemMessageDecoder
        ]


systemMessageDecoder : Decoder { message : String, mediaMaybe : Maybe Media }
systemMessageDecoder =
    D.map2 (\a b -> { message = a, mediaMaybe = b })
        (D.field "message" D.string)
        (D.field "media" (D.maybe mediaDecoder))


mediaDecoder : Decoder Media
mediaDecoder =
    D.oneOf
        [ D.tagged "Image" Image D.url
        , D.tagged "Movie" Movie D.url
        , mediaDecoderOld -- To be removed after migration
        ]


mediaDecoderOld : Decoder Media
mediaDecoderOld =
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


welcome : Item
welcome =
    SystemItem
        { message = "Welcome to Zephyr app! 🚀\n\nThis is Elm-powered multi-service feed reader!\n\nLet's start with configuring column filters above!"
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


initBroker : Broker Item
initBroker =
    Broker.initialize { numSegments = 100, segmentSize = 1000 }
