module Data.Column exposing
    ( Column, ColumnItem(..), Media(..), welcome, new, encode, decoder
    , consumeBroker
    )

{-| Types and functions for columns in Zephyr.

Items stored in List are ordered from latest to oldest.


## Types

@docs Column, ColumnItem, Media, welcome, new, encode, decoder


## Consumer API

@docs consumeBroker

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker, Offset)
import Data.Filter as Filter exposing (Filter)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Url


type alias Column =
    { id : String
    , items : List ColumnItem
    , filters : Array Filter
    , offset : Maybe Offset
    , configOpen : Bool
    , deleteGate : String
    }


type ColumnItem
    = Product Offset Item
    | System
        { message : String
        , mediaMaybe : Maybe Media
        }


type Media
    = Image Url.Url
    | Movie Url.Url


encode : Column -> E.Value
encode c =
    E.object
        [ ( "id", E.string c.id )
        , ( "items", E.list encodeColumnItem c.items )
        , ( "filters", E.array Filter.encode c.filters )
        , ( "offset", E.maybe (E.string << Broker.offsetToString) c.offset )
        ]


encodeColumnItem : ColumnItem -> E.Value
encodeColumnItem cItem =
    case cItem of
        Product offset item ->
            E.tagged2 "Product" (offset |> Broker.offsetToString |> E.string) (Item.encode item)

        System { message, mediaMaybe } ->
            E.tagged "System" <|
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


decoder : Decoder Column
decoder =
    D.map4 (\id items filters offset -> Column id items filters offset False "")
        (D.field "id" D.string)
        (D.field "items" (D.list columnItemDecoder))
        (D.oneOf
            [ D.field "filters" (D.array Filter.decoder)
            , D.succeed Array.empty -- Migration
            ]
        )
        (D.maybeField "offset" offsetDecoder)


columnItemDecoder : Decoder ColumnItem
columnItemDecoder =
    D.oneOf
        [ D.tagged2 "Product" Product offsetDecoder Item.decoder
        , D.tagged "System" System systemMessageDecoder

        -- Old format from Data.Item
        , D.tagged "SystemItem" System systemMessageDecoder
        , D.map System systemMessageDecoder
        ]


offsetDecoder : Decoder Offset
offsetDecoder =
    D.string
        |> D.andThen
            (\s ->
                case Broker.offsetFromString s of
                    Just offset ->
                        D.succeed offset

                    Nothing ->
                        D.fail ("Invalid Broker.Offset: " ++ s)
            )


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
        ]


welcome : String -> Column
welcome id =
    { id = id
    , items =
        [ welcomeItem
        , textOnlyItem "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
        ]
    , filters = Array.empty
    , offset = Nothing
    , configOpen = True
    , deleteGate = ""
    }


textOnlyItem : String -> ColumnItem
textOnlyItem message =
    System { message = message, mediaMaybe = Nothing }


welcomeItem : ColumnItem
welcomeItem =
    System
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


new : String -> Column
new id =
    { id = id
    , items = [ textOnlyItem "New column created! Let's configure filters above!" ]
    , filters = Array.empty
    , offset = Nothing
    , configOpen = True
    , deleteGate = ""
    }


consumeBroker : Int -> Broker Item -> Column -> Column
consumeBroker maxCount broker column =
    case ItemBroker.bulkRead maxCount column.offset broker of
        [] ->
            column

        (( _, newOffset ) :: _) as items ->
            { column
                | offset = Just newOffset
                , items = List.filterMap (applyFilters column.filters) items ++ column.items
            }


applyFilters : Array Filter -> ( Item, Offset ) -> Maybe ColumnItem
applyFilters filters ( item, offset ) =
    if Array.all (Item.matchFilter item) filters then
        Just (Product offset item)

    else
        Nothing
