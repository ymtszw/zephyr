module Data.Column exposing
    ( Column, ColumnItem(..), Media(..), welcome, new, simple, encode, decoder, columnItemLimit
    , Msg(..), PostProcess, Position(..), update, postProcess
    )

{-| Types and functions for columns in Zephyr.

Items stored in List are ordered from latest to oldest.

Now that Columns are backed by Scrolls, they have limit on maximum Items.
Also, number of Items shown depends on runtime clientHeight.

@docs Column, ColumnItem, Media, welcome, new, simple, encode, decoder, columnItemLimit
@docs Msg, PostProcess, Position, update, postProcess

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker, Offset)
import Data.ColumnEditor as ColumnEditor exposing (ColumnEditor(..))
import Data.Filter as Filter exposing (Filter, FilterAtom)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Producer as Producer
import Data.Producer.Discord as Discord
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import File exposing (File)
import File.Select
import Hex
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Scroll exposing (Scroll)
import SelectArray exposing (SelectArray)
import Task
import Url
import View.Parts exposing (itemMinimumHeight)


type alias Column =
    { id : String
    , items : Scroll ColumnItem
    , filters : Array Filter
    , offset : Maybe Offset
    , pinned : Bool
    , recentlyTouched : Bool -- This property may become stale, though it should have no harm
    , configOpen : Bool
    , pendingFilters : Array Filter
    , editors : SelectArray ColumnEditor
    , deleteGate : String
    }


type ColumnItem
    = Product Offset Item
    | System String { message : String, mediaMaybe : Maybe Media }
    | LocalMessage String { message : String }


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

        LocalMessage id { message } ->
            E.tagged2 "LocalMessage" (E.string id) <|
                E.object
                    [ ( "message", E.string message )
                    ]


encodeMedia : Media -> E.Value
encodeMedia media =
    case media of
        Image url ->
            E.tagged "Image" (E.string (Url.toString url))

        Movie url ->
            E.tagged "Movie" (E.string (Url.toString url))


decoder : Int -> Decoder ( Column, Cmd Msg )
decoder clientHeight =
    let
        scrollDecoder id =
            Scroll.decoder (autoAdjustOptions clientHeight)
                (scrollInitOptions id clientHeight)
                columnItemDecoder
    in
    D.do (D.field "id" D.string) <|
        \id ->
            D.do (D.field "items" (scrollDecoder id)) <|
                \( items, sCmd ) ->
                    D.do (D.field "filters" (D.array Filter.decoder)) <|
                        \filters ->
                            D.do (D.maybeField "offset" offsetDecoder) <|
                                \offset ->
                                    -- Migration; use field instead of maybeField later
                                    D.do (D.maybeField "pinned" D.bool |> D.map (Maybe.withDefault False)) <|
                                        \pinned ->
                                            let
                                                c =
                                                    { id = id
                                                    , items = items
                                                    , filters = filters
                                                    , offset = offset
                                                    , pinned = pinned
                                                    , recentlyTouched = False
                                                    , configOpen = False
                                                    , pendingFilters = filters
                                                    , editors = ColumnEditor.filtersToEditors filters
                                                    , deleteGate = ""
                                                    }
                                            in
                                            D.succeed ( c, Cmd.map ScrollMsg sCmd )


columnItemDecoder : Decoder ColumnItem
columnItemDecoder =
    D.oneOf
        [ D.tagged2 "Product" Product offsetDecoder Item.decoder
        , D.tagged2 "System" System D.string <|
            D.map2 (\a b -> { message = a, mediaMaybe = b })
                (D.field "message" D.string)
                (D.field "media" (D.maybe mediaDecoder))
        , D.tagged2 "LocalMessage" LocalMessage D.string <|
            D.map (\a -> { message = a })
                (D.field "message" D.string)
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
                , systemMessage "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
                ]
    in
    ( { id = id
      , items = Scroll.initWith (scrollInitOptions id clientHeight) items
      , filters = Array.empty
      , offset = Nothing
      , pinned = False
      , recentlyTouched = True
      , configOpen = True
      , pendingFilters = Array.empty
      , editors = ColumnEditor.defaultEditors
      , deleteGate = ""
      }
    , newGen
    )


scrollInitOptions : String -> Int -> Scroll.InitOptions
scrollInitOptions id clientHeight =
    let
        base =
            Scroll.defaultOptions ("scroll-" ++ id)

        fillAmount =
            clientHeight // itemMinimumHeight
    in
    { base
        | limit = columnItemLimit
        , baseAmount = round (toFloat fillAmount * baseRatio)
        , tierAmount = round (toFloat fillAmount * tierRatio)
    }


columnItemLimit : Int
columnItemLimit =
    2000


baseRatio : Float
baseRatio =
    1.3


tierRatio : Float
tierRatio =
    2.5


autoAdjustOptions : Int -> Scroll.AutoAdjustOptions
autoAdjustOptions clientHeight =
    { clientHeight = clientHeight
    , baseRatio = baseRatio
    , tierRatio = tierRatio
    }


systemMessage : String -> String -> ColumnItem
systemMessage message id =
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
                systemMessage "New column created! Let's configure filters above!"
    in
    ( { id = id
      , items = Scroll.initWith (scrollInitOptions id clientHeight) [ item ]
      , filters = Array.empty
      , offset = Nothing
      , pinned = False
      , recentlyTouched = True
      , configOpen = True
      , pendingFilters = Array.empty
      , editors = ColumnEditor.defaultEditors
      , deleteGate = ""
      }
    , newGen
    )


simple : Int -> FilterAtom -> String -> Column
simple clientHeight fa id =
    let
        filters =
            Array.fromList [ Filter.Singular fa ]
    in
    { id = id
    , items = Scroll.init (scrollInitOptions id clientHeight)
    , filters = filters
    , offset = Nothing
    , pinned = False
    , recentlyTouched = True
    , configOpen = False
    , pendingFilters = filters
    , editors = ColumnEditor.filtersToEditors filters
    , deleteGate = ""
    }


type Msg
    = ToggleConfig Bool
    | Pin Bool
    | Show
    | Calm
    | AddFilter Filter
    | DelFilter Int
    | AddFilterAtom { filterIndex : Int, atom : FilterAtom }
    | SetFilterAtom { filterIndex : Int, atomIndex : Int, atom : FilterAtom }
    | DelFilterAtom { filterIndex : Int, atomIndex : Int }
    | ConfirmFilter
    | DeleteGateInput String
    | SelectEditor Int
    | EditorToggle Bool
    | EditorInput String
    | EditorReset
    | EditorSubmit Int
    | EditorFileRequest (List String)
    | EditorFileSelected File
    | EditorFileLoaded ( File, String )
    | EditorFileDiscard
    | ScanBroker { broker : Broker Item, maxCount : Int, clientHeight : Int, catchUp : Bool }
    | ScrollMsg Scroll.Msg


type alias PostProcess =
    { cmd : Cmd Msg
    , persist : Bool
    , catchUpId : Maybe String
    , position : Position
    , producerMsg : Maybe Producer.Msg
    , heartstopper : Bool
    }


type Position
    = Auto
    | Bump
    | Keep


postProcess : PostProcess
postProcess =
    { cmd = Cmd.none
    , persist = False
    , catchUpId = Nothing
    , position = Keep
    , producerMsg = Nothing
    , heartstopper = False
    }


update : Msg -> Column -> ( Column, PostProcess )
update msg c =
    case msg of
        ToggleConfig open ->
            pure { c | configOpen = open, pendingFilters = c.filters, deleteGate = "" }

        Pin pinned ->
            ( { c | pinned = pinned, recentlyTouched = True }, { postProcess | persist = True, position = Auto } )

        Show ->
            let
                ( items, sCmd ) =
                    Scroll.update Scroll.Reveal c.items
            in
            ( { c | items = items, recentlyTouched = True }
            , { postProcess | cmd = Cmd.map ScrollMsg sCmd, persist = True, position = Bump }
            )

        Calm ->
            pure { c | recentlyTouched = False }

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
            ( { c | filters = c.pendingFilters, offset = Nothing, items = Scroll.clear c.items, configOpen = False }
            , { postProcess | persist = True, catchUpId = Just c.id }
            )

        DeleteGateInput input ->
            pure { c | deleteGate = input }

        SelectEditor index ->
            pure { c | editors = SelectArray.selectAt index c.editors }

        EditorToggle isActive ->
            pure { c | editors = SelectArray.updateSelected (ColumnEditor.toggleActive isActive) c.editors }

        EditorInput input ->
            pure { c | editors = SelectArray.updateSelected (ColumnEditor.updateBuffer input) c.editors }

        EditorReset ->
            ( { c | editors = SelectArray.updateSelected ColumnEditor.reset c.editors }
            , { postProcess | heartstopper = False }
            )

        EditorSubmit clientHeight ->
            editorSubmit clientHeight c

        EditorFileRequest mimeTypes ->
            ( c, { postProcess | cmd = File.Select.file mimeTypes EditorFileSelected, heartstopper = True } )

        EditorFileSelected file ->
            ( c
            , { postProcess
                | cmd = Task.perform EditorFileLoaded (Task.map (Tuple.pair file) (File.toUrl file))
                , heartstopper = False
              }
            )

        EditorFileLoaded fileTuple ->
            pure { c | editors = SelectArray.updateSelected (ColumnEditor.updateFile (Just fileTuple)) c.editors }

        EditorFileDiscard ->
            pure { c | editors = SelectArray.updateSelected (ColumnEditor.updateFile Nothing) c.editors }

        ScanBroker opts ->
            scanBroker opts c

        ScrollMsg sMsg ->
            let
                ( items, sCmd ) =
                    Scroll.update sMsg c.items
            in
            ( { c | items = items }, { postProcess | cmd = Cmd.map ScrollMsg sCmd } )


pure : Column -> ( Column, PostProcess )
pure c =
    ( c, postProcess )


scanBroker :
    { broker : Broker Item, maxCount : Int, clientHeight : Int, catchUp : Bool }
    -> Column
    -> ( Column, PostProcess )
scanBroker { broker, maxCount, clientHeight, catchUp } c =
    case ItemBroker.bulkRead maxCount c.offset broker of
        [] ->
            pure c

        (( _, newOffset ) :: _) as items ->
            let
                ( c_, pp ) =
                    case ( catchUp, List.filterMap (applyFilters c.filters) items ) of
                        ( True, [] ) ->
                            ( c, { postProcess | persist = True, catchUpId = Just c.id } )

                        ( True, newItems ) ->
                            let
                                ( items_, sCmd ) =
                                    prependItems (autoAdjustOptions clientHeight) newItems c.items
                            in
                            -- Do not bump, nor flash Column during catchUp
                            ( { c | items = items_ }
                            , { postProcess | cmd = Cmd.map ScrollMsg sCmd, persist = True, catchUpId = Just c.id }
                            )

                        ( False, [] ) ->
                            ( c, { postProcess | persist = True } )

                        ( False, newItems ) ->
                            let
                                ( items_, sCmd ) =
                                    prependItems (autoAdjustOptions clientHeight) newItems c.items
                            in
                            ( { c | items = items_, recentlyTouched = True }
                            , if c.pinned then
                                -- Do not bump Pinned Column
                                { postProcess | cmd = Cmd.map ScrollMsg sCmd, persist = True }

                              else
                                { postProcess | cmd = Cmd.map ScrollMsg sCmd, persist = True, position = Bump }
                            )
            in
            ( { c_ | offset = Just newOffset }, pp )


applyFilters : Array Filter -> ( Item, Offset ) -> Maybe ColumnItem
applyFilters filters ( item, offset ) =
    if Array.isEmpty filters then
        -- "No filter" (e.g. pass all) is not allowed
        Nothing

    else if Array.all (Item.matchFilter item) filters then
        Just (Product offset item)

    else
        Nothing


prependItems : Scroll.AutoAdjustOptions -> List ColumnItem -> Scroll ColumnItem -> ( Scroll ColumnItem, Cmd Scroll.Msg )
prependItems opts newItems items =
    items
        |> Scroll.prependList newItems
        |> Scroll.update (Scroll.AdjustReq opts)


editorSubmit : Int -> Column -> ( Column, PostProcess )
editorSubmit clientHeight c =
    case SelectArray.selected c.editors of
        DiscordMessageEditor { channelId, file } { buffer } ->
            if String.isEmpty buffer && file == Nothing then
                pure c

            else
                let
                    postMsg =
                        Producer.DiscordMsg <|
                            Discord.Post
                                { channelId = channelId
                                , message = Just buffer
                                , file = Maybe.map Tuple.first file
                                }
                in
                ( { c | editors = SelectArray.updateSelected ColumnEditor.reset c.editors }
                , { postProcess | producerMsg = Just postMsg }
                )

        LocalMessageEditor { buffer } ->
            if String.isEmpty buffer then
                pure c

            else
                saveLocalMessage clientHeight buffer c


saveLocalMessage : Int -> String -> Column -> ( Column, PostProcess )
saveLocalMessage clientHeight buffer c =
    let
        prevId =
            case Scroll.pop c.items of
                ( Just (Product offset _), _ ) ->
                    Broker.offsetToString offset

                ( Just (System id _), _ ) ->
                    id

                ( Just (LocalMessage id _), _ ) ->
                    id

                ( Nothing, _ ) ->
                    "root"

        ( newItems, sMsg ) =
            prependItems (autoAdjustOptions clientHeight)
                [ localMessage prevId buffer ]
                c.items
    in
    ( { c
        | items = newItems
        , editors = SelectArray.updateSelected ColumnEditor.reset c.editors
      }
    , { postProcess | cmd = Cmd.map ScrollMsg sMsg, persist = True }
    )


localMessage : String -> String -> ColumnItem
localMessage prevId message =
    LocalMessage (localMessageId prevId) { message = message }


localMessageId : String -> String
localMessageId prevId =
    let
        initialSeqHex =
            String.repeat localMessageHexLen "0"
    in
    if String.startsWith localMessagePrefix prevId then
        let
            attachedTo =
                String.dropLeft (localMessagePrefixLen + localMessageHexLen) prevId
        in
        String.join ""
            [ localMessagePrefix
            , case prevId |> String.dropLeft localMessagePrefixLen |> String.left localMessageHexLen |> Hex.fromString of
                Ok prevSeq ->
                    String.padLeft localMessageHexLen '0' (Hex.toString (prevSeq + 1))

                Err _ ->
                    initialSeqHex
            , attachedTo
            ]

    else
        localMessagePrefix ++ initialSeqHex ++ prevId


localMessagePrefix : String
localMessagePrefix =
    "lm"


localMessagePrefixLen : Int
localMessagePrefixLen =
    String.length localMessagePrefix


localMessageHexLen : Int
localMessageHexLen =
    4
