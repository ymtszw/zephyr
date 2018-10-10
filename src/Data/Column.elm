module Data.Column exposing (Column, Filter, decoder, encoder, welcome)

import Array exposing (Array)
import Data.Item as Item exposing (Item)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


type alias Column =
    { id : String
    , items : List Item
    , filters : Array Filter
    , configOpen : Bool
    }


{-| Filter to narrow down Items flowing into a Column.

An Array of Filters works in logical "and" manner.
If newly arriving Item meets ALL Filters in the list,
it enters the Column. Otherwise rejected.

-}
type Filter
    = ByMessage String
    | ByMedia MediaType
    | ByMetadata MetadataFilter
    | Or (List Filter)


type MediaType
    = None
    | Image
    | Movie


type MetadataFilter
    = IsDiscord
    | OfDiscordGuild String
    | OfDiscordChannel String
    | OfDiscordUser String
    | IsDefault


decoder : Decoder Column
decoder =
    D.map4 Column
        (D.field "id" D.string)
        (D.field "items" (D.list Item.decoder))
        (D.oneOf
            [ D.field "filters" (D.array filterDecoder)
            , D.succeed Array.empty -- Migration
            ]
        )
        (D.succeed False)


filterDecoder : Decoder Filter
filterDecoder =
    D.oneOf
        [ D.tagged "ByMessage" ByMessage D.string
        , D.tagged "ByMedia" ByMedia mediaTypeDecoder
        , D.tagged "ByMetadata" ByMetadata metadataFilterDecoder
        , D.tagged "Or" Or (D.list (D.lazy (\_ -> filterDecoder)))
        ]


mediaTypeDecoder : Decoder MediaType
mediaTypeDecoder =
    D.oneOf [ D.tag "None" None, D.tag "Image" Image, D.tag "Movie" Movie ]


metadataFilterDecoder : Decoder MetadataFilter
metadataFilterDecoder =
    D.oneOf
        [ D.tag "IsDiscord" IsDiscord
        , D.tagged "OfDiscordGuild" OfDiscordGuild D.string
        , D.tagged "OfDiscordChannel" OfDiscordChannel D.string
        , D.tagged "OfDiscordUser" OfDiscordUser D.string
        , D.tag "IsDefault" IsDefault
        ]


encoder : Column -> E.Value
encoder { id, items, filters } =
    E.object
        [ ( "id", E.string id )
        , ( "items", E.list Item.encoder items )
        , ( "filters", E.array encodeFilter filters )
        ]


encodeFilter : Filter -> E.Value
encodeFilter filter =
    case filter of
        ByMessage query ->
            E.tagged "ByMessage" (E.string query)

        ByMedia mediaType ->
            E.tagged "ByMedia" (encodeMediaType mediaType)

        ByMetadata metadataFilter ->
            E.tagged "ByMetadata" (encodeMetadataFilter metadataFilter)

        Or filters ->
            E.tagged "Or" (E.list encodeFilter filters)


encodeMediaType : MediaType -> E.Value
encodeMediaType mediaType =
    case mediaType of
        None ->
            E.tag "None"

        Image ->
            E.tag "Image"

        Movie ->
            E.tag "Movie"


encodeMetadataFilter : MetadataFilter -> E.Value
encodeMetadataFilter metadataFilter =
    case metadataFilter of
        IsDiscord ->
            E.tag "IsDiscord"

        OfDiscordGuild guildId ->
            E.tagged "OfDiscordGuild" (E.string guildId)

        OfDiscordChannel channelId ->
            E.tagged "OfDiscordChannel" (E.string channelId)

        OfDiscordUser userId ->
            E.tagged "OfDiscordUser" (E.string userId)

        IsDefault ->
            E.tag "IsDefault"


welcome : String -> Column
welcome id =
    { id = id
    , items =
        [ Item.welcome
        , Item.textOnly "Text only message is also possible!"
        , Item.textOnly "URLs in message are automatically linkified by default.\nSource code of this project can be found at https://github.com/ymtszw/zephyr !\nAnd zephyr is powered by outstanding Elm language! https://elm-lang.org\nAlso, zephyr utilizes ServiceWorker and other technologies for Progressive Web App! https://developers.google.com/web/progressive-web-apps/\nIt works offline! Try it out!."
        , Item.textOnly "Design is obviously inspired by Tweetdeck. Scrollbar style is only applied in webkit-family browsers."
        , Item.textOnly "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        ]
            |> List.repeat 2
            |> List.concat
    , filters = Array.empty
    , configOpen = False
    }
