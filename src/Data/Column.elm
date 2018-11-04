module Data.Column exposing
    ( Column, ColumnItem(..), Media(..), welcome, new, encode, decoder
    , Msg(..), update, consumeBroker
    )

{-| Types and functions for columns in Zephyr.

Items stored in List are ordered from latest to oldest.


## Types

@docs Column, ColumnItem, Media, welcome, new, encode, decoder


## Component API

@docs Msg, update, consumeBroker

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker, Offset)
import Data.Filter as Filter exposing (Filter, FilterAtom)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.UniqueId as UniqueId
import Extra exposing (pure)
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
    , pendingFilters : Array Filter
    , deleteGate : String
    }


type ColumnItem
    = Product Offset Item
    | System String { message : String, mediaMaybe : Maybe Media }


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

        System id { message, mediaMaybe } ->
            E.tagged2 "System" (E.string id) <|
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
    D.map4 (\id items filters offset -> Column id items filters offset False filters "")
        (D.field "id" D.string)
        (D.field "items" (D.leakyList columnItemDecoder))
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
        , D.tagged2 "System" System D.string systemMessageDecoder
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


welcome : UniqueId.Generator -> String -> ( Column, UniqueId.Generator )
welcome idGen id =
    let
        ( items, newGen ) =
            UniqueId.sequence "systemMessage" idGen <|
                [ welcomeItem
                , textOnlyItem "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
                ]
    in
    ( { id = id
      , items = items
      , filters = Array.empty
      , offset = Nothing
      , configOpen = True
      , pendingFilters = Array.empty
      , deleteGate = ""
      }
    , newGen
    )


textOnlyItem : String -> String -> ColumnItem
textOnlyItem message id =
    System id { message = message, mediaMaybe = Nothing }


welcomeItem : String -> ColumnItem
welcomeItem id =
    System
        id
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


new : UniqueId.Generator -> String -> ( Column, UniqueId.Generator )
new idGen id =
    let
        ( item, newGen ) =
            UniqueId.genAndMap "systemMessage" idGen <|
                textOnlyItem "New column created! Let's configure filters above!"
    in
    ( { id = id
      , items = [ item ]
      , filters = Array.empty
      , offset = Nothing
      , configOpen = True
      , pendingFilters = Array.empty
      , deleteGate = ""
      }
    , newGen
    )


type Msg
    = ToggleConfig Bool
    | AddFilter Filter
    | DelFilter Int
    | AddFilterAtom { filterIndex : Int, atom : FilterAtom }
    | SetFilterAtom { filterIndex : Int, atomIndex : Int, atom : FilterAtom }
    | DelFilterAtom { filterIndex : Int, atomIndex : Int }
    | ConfirmFilter
    | DeleteGateInput String


update : Msg -> Column -> ( Column, Cmd Msg, Bool )
update msg c =
    case msg of
        ToggleConfig open ->
            pure { c | configOpen = open, pendingFilters = c.filters, deleteGate = "" }

        AddFilter filter ->
            pure { c | pendingFilters = Array.push filter c.pendingFilters }

        DelFilter index ->
            pure { c | pendingFilters = Array.removeAt index c.pendingFilters }

        AddFilterAtom { filterIndex, atom } ->
            pure { c | pendingFilters = Array.update filterIndex (Filter.append atom) c.pendingFilters }

        SetFilterAtom { filterIndex, atomIndex, atom } ->
            pure { c | pendingFilters = Array.update filterIndex (Filter.setAt atomIndex atom) c.pendingFilters }

        DelFilterAtom { filterIndex, atomIndex } ->
            let
                atomOrFilterDeleted =
                    case Array.get filterIndex c.pendingFilters of
                        Just filter ->
                            case Filter.removeAt atomIndex filter of
                                Just newFilter ->
                                    Array.set filterIndex newFilter c.pendingFilters

                                Nothing ->
                                    Array.removeAt filterIndex c.pendingFilters

                        Nothing ->
                            c.pendingFilters
            in
            pure { c | pendingFilters = atomOrFilterDeleted }

        ConfirmFilter ->
            ( { c | filters = c.pendingFilters, offset = Nothing, items = [] }, Cmd.none, True )

        DeleteGateInput input ->
            pure { c | deleteGate = input }


consumeBroker : Int -> Broker Item -> Column -> ( Column, Bool )
consumeBroker maxCount broker column =
    case ItemBroker.bulkRead maxCount column.offset broker of
        [] ->
            ( column, False )

        (( _, newOffset ) :: _) as items ->
            ( { column
                | offset = Just newOffset
                , items = List.filterMap (applyFilters column.filters) items ++ column.items
              }
            , True
            )


applyFilters : Array Filter -> ( Item, Offset ) -> Maybe ColumnItem
applyFilters filters ( item, offset ) =
    if Array.isEmpty filters then
        -- "No filter" (e.g. pass all) is not allowed
        Nothing

    else if Array.all (Item.matchFilter item) filters then
        Just (Product offset item)

    else
        Nothing
