module Data.Column exposing
    ( Column, ColumnItem(..), Media(..), welcome, new, simple, encode, encodeColumnItem, decoder, columnItemLimit
    , Msg(..), PostProcess, Position(..), update, postProcess
    , editorId, itemNotFound
    )

{-| Types and functions for columns in Zephyr.

Items stored in List are ordered from latest to oldest.

Now that Columns are backed by Scrolls, they have limit on maximum Items.
Also, number of Items shown depends on runtime clientHeight.

@docs Column, ColumnItem, Media, welcome, new, simple, encode, encodeColumnItem, decoder, columnItemLimit
@docs Msg, PostProcess, Position, update, postProcess
@docs editorId, itemNotFound

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker, Offset)
import Browser.Dom
import Data.ColumnEditor as ColumnEditor exposing (ColumnEditor(..))
import Data.Filter as Filter exposing (Filter, FilterAtom)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Producer.Discord as Discord
import Data.ProducerRegistry as ProducerRegistry
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
import StringExtra
import Task
import Url
import View.Parts exposing (columnHeaderHeight, columnItemMinimumHeight)


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
    , editorSeq : Int -- Force triggering DOM generation when incremented; workaround for https://github.com/mdgriffith/elm-ui/issues/5
    , userActionOnEditor : ColumnEditor.UserAction
    , deleteGate : String -- TODO Remove
    }


type ColumnItem
    = Product Offset Item
    | SystemMessage SystemMessageRecord
    | LocalMessage LocalMessageRecord


type alias SystemMessageRecord =
    { id : String
    , message : String
    , mediaMaybe : Maybe Media
    }


type alias LocalMessageRecord =
    { id : String
    , message : String
    }


type Media
    = Image Url.Url
    | Video Url.Url


itemNotFound : ColumnItem
itemNotFound =
    LocalMessage { id = "(Not Found)", message = "Target item cannot be found in the column!" }


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

        SystemMessage { id, message, mediaMaybe } ->
            E.tagged "SystemMessage" <|
                E.object
                    [ ( "id", E.string id )
                    , ( "message", E.string message )
                    , ( "media", E.maybe encodeMedia mediaMaybe )
                    ]

        LocalMessage { id, message } ->
            E.tagged "LocalMessage" <|
                E.object
                    [ ( "id", E.string id )
                    , ( "message", E.string message )
                    ]


encodeMedia : Media -> E.Value
encodeMedia media =
    case media of
        Image url ->
            E.tagged "Image" (E.string (Url.toString url))

        Video url ->
            E.tagged "Video" (E.string (Url.toString url))


decoder : Int -> Decoder ( Column, Cmd Msg )
decoder clientHeight =
    let
        scrollDecoder id =
            Scroll.decoder (scrollInitOptions id clientHeight) columnItemDecoder
    in
    D.do (D.field "id" D.string) <|
        \id ->
            D.do (D.field "items" (scrollDecoder id)) <|
                \( items, sCmd ) ->
                    D.do (D.field "filters" (D.array Filter.decoder)) <|
                        \filters ->
                            D.do (D.maybeField "offset" offsetDecoder) <|
                                \offset ->
                                    -- Migration; use field instead of optionField later
                                    D.do (D.optionField "pinned" D.bool False) <|
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
                                                    , editorSeq = 0
                                                    , userActionOnEditor = ColumnEditor.OutOfFocus
                                                    , deleteGate = ""
                                                    }
                                            in
                                            D.succeed ( c, Cmd.map ScrollMsg sCmd )


columnItemDecoder : Decoder ColumnItem
columnItemDecoder =
    D.oneOf
        [ D.tagged2 "Product" Product offsetDecoder Item.decoder
        , D.tagged "SystemMessage" SystemMessage <|
            D.map3 SystemMessageRecord
                (D.field "id" D.string)
                (D.field "message" D.string)
                (D.field "media" (D.maybe mediaDecoder))
        , D.tagged "LocalMessage" LocalMessage <|
            D.map2 LocalMessageRecord
                (D.field "id" D.string)
                (D.field "message" D.string)
        , -- Old formats
          let
            fromOld id ( ms, md ) =
                SystemMessage { id = id, message = ms, mediaMaybe = md }
          in
          D.tagged2 "System" fromOld D.string <|
            D.map2 Tuple.pair (D.field "message" D.string) (D.field "media" (D.maybe mediaDecoder))
        , let
            fromOld id ms =
                LocalMessage { id = id, message = ms }
          in
          D.tagged2 "LocalMessage" fromOld D.string (D.field "message" D.string)
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
        , D.tagged "Video" Video D.url
        , -- Old formats
          D.tagged "Movie" Video D.url
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
      , editorSeq = 0
      , userActionOnEditor = ColumnEditor.OutOfFocus
      , deleteGate = ""
      }
    , newGen
    )


scrollInitOptions : String -> Int -> Scroll.InitOptions
scrollInitOptions id clientHeight =
    let
        opts =
            Scroll.defaultOptions
                { id = "scroll-" ++ id
                , boundingHeight = boundingHeight clientHeight
                , minimumItemHeight = columnItemMinimumHeight
                }
    in
    { opts | limit = columnItemLimit, baseRatio = baseRatio, tierRatio = tierRatio }


boundingHeight : Int -> Int
boundingHeight clientHeight =
    clientHeight - columnHeaderHeight


columnItemLimit : Int
columnItemLimit =
    2000


baseRatio : Float
baseRatio =
    2.0


tierRatio : Float
tierRatio =
    4.0


systemMessage : String -> String -> ColumnItem
systemMessage message id =
    SystemMessage { id = id, message = message, mediaMaybe = Nothing }


welcomeItem : String -> ColumnItem
welcomeItem id =
    SystemMessage
        { id = id
        , message = "Welcome to Zephyr app! 🚀\n\nThis is Elm-powered multi-service feed reader!\n\nLet's start with configuring column filters above!"
        , mediaMaybe = Just (Image welcomeGif)
        }


welcomeGif : Url.Url
welcomeGif =
    StringExtra.toUrlUnsafe "https://cdn.dribbble.com/users/27231/screenshots/2432051/welcome.gif"


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
      , editorSeq = 0
      , userActionOnEditor = ColumnEditor.OutOfFocus
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
    , editorSeq = 0
    , userActionOnEditor = ColumnEditor.OutOfFocus
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
    | EditorInteracted ColumnEditor.UserAction
    | EditorInput String
    | EditorReset
    | EditorSubmit
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
    , producerMsg : Maybe ProducerRegistry.Msg
    , heartstopper : Bool -- TODO Remove
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


update : Bool -> Msg -> Column -> ( Column, PostProcess )
update isVisible msg c =
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
            pure { c | editors = SelectArray.selectAt index c.editors, editorSeq = c.editorSeq + 1 }

        EditorInteracted action ->
            let
                cmd =
                    case action of
                        ColumnEditor.OutOfFocus ->
                            -- Calm is just a replacement for NoOp
                            Task.attempt (always Calm) (Browser.Dom.blur (editorId c.id))

                        _ ->
                            Cmd.none
            in
            ( { c | userActionOnEditor = action }, { postProcess | heartstopper = False, cmd = cmd } )

        EditorInput input ->
            pure { c | editors = SelectArray.updateSelected (ColumnEditor.updateBuffer input) c.editors }

        EditorReset ->
            ( { c
                | editors = SelectArray.updateSelected ColumnEditor.reset c.editors
                , editorSeq = c.editorSeq + 1
              }
            , { postProcess | heartstopper = False }
            )

        EditorSubmit ->
            editorSubmit c

        EditorFileRequest mimeTypes ->
            ( c, { postProcess | cmd = File.Select.file mimeTypes EditorFileSelected, heartstopper = True } )

        EditorFileSelected file ->
            ( { c | userActionOnEditor = ColumnEditor.Authoring }
            , { postProcess
                | cmd = Task.perform EditorFileLoaded (Task.map (Tuple.pair file) (File.toUrl file))
                , heartstopper = False
              }
            )

        EditorFileLoaded fileTuple ->
            pure { c | editors = SelectArray.updateSelected (ColumnEditor.updateFile (Just fileTuple)) c.editors }

        EditorFileDiscard ->
            ( { c | editors = SelectArray.updateSelected (ColumnEditor.updateFile Nothing) c.editors }
            , { postProcess | heartstopper = False }
            )

        ScanBroker opts ->
            scanBroker isVisible opts c

        ScrollMsg sMsg ->
            if isVisible then
                let
                    ( items, sCmd ) =
                        Scroll.update sMsg c.items
                in
                ( { c | items = items }, { postProcess | cmd = Cmd.map ScrollMsg sCmd } )

            else
                pure c


pure : Column -> ( Column, PostProcess )
pure c =
    ( c, postProcess )


scanBroker :
    Bool
    -> { broker : Broker Item, maxCount : Int, clientHeight : Int, catchUp : Bool }
    -> Column
    -> ( Column, PostProcess )
scanBroker isVisible { broker, maxCount, clientHeight, catchUp } c_ =
    let
        adjustOpts =
            { isVisible = isVisible, clientHeight = clientHeight, hasNewItem = False }
    in
    case ItemBroker.bulkRead maxCount c_.offset broker of
        [] ->
            adjustScroll adjustOpts ( c_, postProcess )

        (( _, newOffset ) :: _) as items ->
            let
                c =
                    { c_ | offset = Just newOffset }

                ppBase =
                    { postProcess | persist = True }
            in
            case ( catchUp, List.filterMap (applyFilters c.filters) items ) of
                ( True, [] ) ->
                    adjustScroll adjustOpts ( c, { ppBase | catchUpId = Just c.id } )

                ( True, newItems ) ->
                    adjustScroll { adjustOpts | hasNewItem = True } <|
                        -- Do not bump, nor flash Column during catchUp
                        ( { c | items = Scroll.prependList newItems c.items }
                        , { ppBase | catchUpId = Just c.id }
                        )

                ( False, [] ) ->
                    adjustScroll adjustOpts ( c, ppBase )

                ( False, newItems ) ->
                    adjustScroll { adjustOpts | hasNewItem = True } <|
                        ( { c | items = Scroll.prependList newItems c.items, recentlyTouched = True }
                        , if c.pinned then
                            -- Do not bump Pinned Column
                            ppBase

                          else
                            { ppBase | position = Bump }
                        )


adjustScroll :
    { isVisible : Bool, clientHeight : Int, hasNewItem : Bool }
    -> ( Column, PostProcess )
    -> ( Column, PostProcess )
adjustScroll { isVisible, clientHeight, hasNewItem } ( c, pp ) =
    let
        ( items, sCmd ) =
            if hasNewItem then
                Scroll.update Scroll.NewItem c.items

            else if isVisible then
                Scroll.update (Scroll.AdjustReq (boundingHeight clientHeight)) c.items

            else
                ( c.items, Cmd.none )
    in
    ( { c | items = items }, { pp | cmd = Cmd.map ScrollMsg sCmd } )


applyFilters : Array Filter -> ( Item, Offset ) -> Maybe ColumnItem
applyFilters filters ( item, offset ) =
    if Array.isEmpty filters then
        -- "No filter" (e.g. pass all) is not allowed
        Nothing

    else if Array.all (Item.matchFilter item) filters then
        Just (Product offset item)

    else
        Nothing


editorSubmit : Column -> ( Column, PostProcess )
editorSubmit c =
    case SelectArray.selected c.editors of
        DiscordMessageEditor { channelId, buffer, file } ->
            if String.isEmpty buffer && file == Nothing then
                pure c

            else
                let
                    postMsg =
                        ProducerRegistry.DiscordMsg <|
                            Discord.Post
                                { channelId = channelId
                                , message = Just buffer
                                , file = Maybe.map Tuple.first file
                                }
                in
                ( { c
                    | editors = SelectArray.updateSelected ColumnEditor.reset c.editors
                    , editorSeq = c.editorSeq + 1
                    , userActionOnEditor = ColumnEditor.OutOfFocus
                  }
                , { postProcess | catchUpId = Just c.id, producerMsg = Just postMsg }
                )

        LocalMessageEditor buffer ->
            if String.isEmpty buffer then
                pure c

            else
                saveLocalMessage buffer c


saveLocalMessage : String -> Column -> ( Column, PostProcess )
saveLocalMessage buffer c =
    let
        prevId =
            case Scroll.pop c.items of
                ( Just (Product offset _), _ ) ->
                    Broker.offsetToString offset

                ( Just (SystemMessage { id }), _ ) ->
                    id

                ( Just (LocalMessage { id }), _ ) ->
                    id

                ( Nothing, _ ) ->
                    "root"

        ( newItems, sCmd ) =
            c.items
                |> Scroll.push (localMessage prevId buffer)
                |> Scroll.update Scroll.NewItem
    in
    ( { c
        | items = newItems
        , editors = SelectArray.updateSelected ColumnEditor.reset c.editors
        , userActionOnEditor = ColumnEditor.OutOfFocus
        , editorSeq = c.editorSeq + 1
      }
    , { postProcess | cmd = Cmd.map ScrollMsg sCmd, persist = True }
    )


localMessage : String -> String -> ColumnItem
localMessage prevId message =
    LocalMessage { id = localMessageId prevId, message = message }


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


editorId : String -> String
editorId id =
    "newMessageEditor_" ++ id
