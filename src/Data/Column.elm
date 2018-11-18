module Data.Column exposing
    ( Column, ColumnItem(..), Media(..), welcome, new, simple, encode, decoder, adjustScroll, columnItemLimit
    , Msg(..), PostProcess, update, consumeBroker
    )

{-| Types and functions for columns in Zephyr.

Items stored in List are ordered from latest to oldest.

Now that Columns are backed by Scrolls, they have limit on maximum Items.
Also, number of Items shown depends on runtime clientHeight.

@docs Column, ColumnItem, Media, welcome, new, simple, encode, decoder, adjustScroll, columnItemLimit
@docs Msg, PostProcess, update, consumeBroker

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker, Offset)
import Data.Filter as Filter exposing (Filter, FilterAtom)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Scroll exposing (Scroll)
import Url
import View.Parts exposing (itemMinimumHeight)


type alias Column =
    { id : String
    , items : Scroll ColumnItem
    , filters : Array Filter
    , offset : Maybe Offset
    , pinned : Bool
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
        , ( "items", Scroll.encode encodeColumnItem c.items )
        , ( "filters", E.array Filter.encode c.filters )
        , ( "offset", E.maybe (E.string << Broker.offsetToString) c.offset )
        , ( "pinned", E.bool c.pinned )
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


decoder : Int -> Decoder Column
decoder clientHeight =
    D.do (D.field "id" D.string) <|
        \id ->
            D.do (D.field "items" (Scroll.decoder (scrollOptions id clientHeight) columnItemDecoder)) <|
                \items ->
                    D.do (D.field "filters" (D.array Filter.decoder)) <|
                        \filters ->
                            D.do (D.maybeField "offset" offsetDecoder) <|
                                \offset ->
                                    -- Migration; use field instead of maybeField later
                                    D.do (D.maybeField "pinned" D.bool |> D.map (Maybe.withDefault False)) <|
                                        \pinned ->
                                            D.succeed (Column id items filters offset pinned False filters "")


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


welcome : Int -> UniqueIdGen -> String -> ( Column, UniqueIdGen )
welcome clientHeight idGen id =
    let
        ( items, newGen ) =
            UniqueIdGen.sequence UniqueIdGen.systemMessagePrefix idGen <|
                [ welcomeItem
                , textOnlyItem "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
                ]
    in
    ( { id = id
      , items = Scroll.initWith (scrollOptions id clientHeight) items
      , filters = Array.empty
      , offset = Nothing
      , pinned = False
      , configOpen = True
      , pendingFilters = Array.empty
      , deleteGate = ""
      }
    , newGen
    )


scrollOptions : String -> Int -> Scroll.Options
scrollOptions id clientHeight =
    let
        base =
            Scroll.defaultOptions ("scroll-" ++ id)

        baseAmount =
            columnBaseAmount clientHeight
    in
    { base | limit = columnItemLimit, baseAmount = baseAmount, tierAmount = baseAmount }


columnItemLimit : Int
columnItemLimit =
    2000


columnBaseAmount : Int -> Int
columnBaseAmount clientHeight =
    clientHeight // itemMinimumHeight


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


new : Int -> UniqueIdGen -> String -> ( Column, UniqueIdGen )
new clientHeight idGen id =
    let
        ( item, newGen ) =
            UniqueIdGen.genAndMap UniqueIdGen.systemMessagePrefix idGen <|
                textOnlyItem "New column created! Let's configure filters above!"
    in
    ( { id = id
      , items = Scroll.initWith (scrollOptions id clientHeight) [ item ]
      , filters = Array.empty
      , offset = Nothing
      , pinned = False
      , configOpen = True
      , pendingFilters = Array.empty
      , deleteGate = ""
      }
    , newGen
    )


simple : Int -> FilterAtom -> String -> Column
simple clientHeight fa id =
    { id = id
    , items = Scroll.init (scrollOptions id clientHeight)
    , filters = Array.fromList [ Filter.Singular fa ]
    , offset = Nothing
    , pinned = False
    , configOpen = False
    , pendingFilters = Array.fromList [ Filter.Singular fa ]
    , deleteGate = ""
    }


adjustScroll : Int -> Column -> Column
adjustScroll clientHeight c =
    let
        baseAmount =
            columnBaseAmount clientHeight
    in
    { c | items = c.items |> Scroll.setBaseAmount baseAmount |> Scroll.setTierAmount baseAmount }


type Msg
    = ToggleConfig Bool
    | Pin Bool
    | AddFilter Filter
    | DelFilter Int
    | AddFilterAtom { filterIndex : Int, atom : FilterAtom }
    | SetFilterAtom { filterIndex : Int, atomIndex : Int, atom : FilterAtom }
    | DelFilterAtom { filterIndex : Int, atomIndex : Int }
    | ConfirmFilter
    | DeleteGateInput String
    | ScrollMsg Scroll.Msg


type alias PostProcess =
    { cmd : Cmd Msg
    , persist : Bool
    , catchUpId : Maybe String
    }


update : Msg -> Column -> ( Column, PostProcess )
update msg c =
    case msg of
        ToggleConfig open ->
            pure { c | configOpen = open, pendingFilters = c.filters, deleteGate = "" }

        Pin pinned ->
            ( { c | pinned = pinned }, PostProcess Cmd.none True Nothing )

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
            ( { c | filters = c.pendingFilters, offset = Nothing, items = Scroll.clear c.items }
            , PostProcess Cmd.none True (Just c.id)
            )

        DeleteGateInput input ->
            pure { c | deleteGate = input }

        ScrollMsg sMsg ->
            let
                ( items, cmd ) =
                    Scroll.update sMsg c.items
            in
            ( { c | items = items }, PostProcess (Cmd.map ScrollMsg cmd) False Nothing )


pure : Column -> ( Column, PostProcess )
pure c =
    ( c, PostProcess Cmd.none False Nothing )


consumeBroker : Int -> Broker Item -> Column -> ( Column, Bool )
consumeBroker maxCount broker column =
    case ItemBroker.bulkRead maxCount column.offset broker of
        [] ->
            ( column, False )

        (( _, newOffset ) :: _) as items ->
            ( { column
                | offset = Just newOffset
                , items = Scroll.prependList (List.filterMap (applyFilters column.filters) items) column.items
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
