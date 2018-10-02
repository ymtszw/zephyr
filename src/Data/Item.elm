module Data.Item exposing (Item, Media(..), decoder, encoder, textOnly, welcome)

import Element.Font
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Url


type alias Item =
    -- TODO require broker-generated ID
    { message : String
    , mediaMaybe : Maybe Media
    , metadata : Metadata
    }


type Metadata
    = DiscordMetadata
        { guildName : String
        , channelName : String
        , authorName : String
        }
    | DefaultMetadata


textOnly : String -> Item
textOnly message =
    Item message Nothing DefaultMetadata


type Media
    = Image Url.Url
    | Movie Url.Url


decoder : Decoder Item
decoder =
    D.map3 Item
        (D.field "message" D.string)
        (D.field "media" (D.maybe mediaDecoder))
        (D.oneOf
            [ D.field "metadata" metadataDecoder
            , D.succeed DefaultMetadata -- Fallback
            ]
        )


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


metadataDecoder : Decoder Metadata
metadataDecoder =
    D.oneOf
        [ D.when (D.field "tag" D.string) ((==) "DiscordMetadata") <|
            D.map3 (\gn cn an -> DiscordMetadata { guildName = gn, channelName = cn, authorName = an })
                (D.field "guildName" D.string)
                (D.field "channelName" D.string)
                (D.field "authorName" D.string)
        , D.when (D.field "tag" D.string) ((==) "DefaultMetadata") <| D.succeed DefaultMetadata
        ]


encoder : Item -> E.Value
encoder { message, mediaMaybe, metadata } =
    E.object
        [ ( "message", E.string message )
        , ( "media", mediaMaybe |> Maybe.map mediaEncoder |> Maybe.withDefault E.null )
        , ( "metadata", encodeMetadata metadata )
        ]


mediaEncoder : Media -> E.Value
mediaEncoder media =
    case media of
        Image url ->
            E.string ("IMAGE" ++ Url.toString url)

        Movie url ->
            E.string ("MOVIE" ++ Url.toString url)


encodeMetadata : Metadata -> E.Value
encodeMetadata metadata =
    case metadata of
        DiscordMetadata dmd ->
            E.object
                [ ( "tag", E.string "DiscordMetadata" )
                , ( "guildName", E.string dmd.guildName )
                , ( "channelName", E.string dmd.channelName )
                , ( "authorName", E.string dmd.authorName )
                ]

        DefaultMetadata ->
            E.object [ ( "tag", E.string "DefaultMetadata" ) ]


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
    , metadata = DefaultMetadata
    }
