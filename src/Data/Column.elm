module Data.Column exposing
    ( Column, Id, ColumnItem(..), Media(..), encode, encodeColumnItem, decoder, columnItemLimit
    , new, generator, emptyGenerator, welcomeGenerator, simpleGenerator
    , Msg(..), ScanOptions, PostProcess, Position(..), update, postProcess
    , minimumItemHeight, editorId, itemNotFound
    , getId, getItems, getFilters, getSources, getOffset, getPinned, getRecentlyTouched, getConfigOpen, getPendingFilters, getEditors, getEditorSeq, getUserActionOnEditor
    , setId, setItems, setFilters, setSources, setOffset, setPinned, setRecentlyTouched, setConfigOpen, setPendingFilters, setEditors, setEditorSeq, setUserActionOnEditor
    )

{-| Types and functions for columns in Zephyr.

Items stored in List are ordered from latest to oldest.

Now that Columns are backed by Scrolls, they have limit on maximum Items.
Also, number of Items shown depends on runtime clientHeight.

@docs Column, Id, ColumnItem, Media, encode, encodeColumnItem, decoder, columnItemLimit
@docs new, generator, emptyGenerator, welcomeGenerator, simpleGenerator
@docs Msg, ScanOptions, PostProcess, Position, update, postProcess
@docs minimumItemHeight, editorId, itemNotFound
@docs getId, getItems, getFilters, getSources, getOffset, getPinned, getRecentlyTouched, getConfigOpen, getPendingFilters, getEditors, getEditorSeq, getUserActionOnEditor
@docs setId, setItems, setFilters, setSources, setOffset, setPinned, setRecentlyTouched, setConfigOpen, setPendingFilters, setEditors, setEditorSeq, setUserActionOnEditor

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker, Offset)
import Browser.Dom
import Data.Column.IdGenerator exposing (idGenerator)
import Data.Column.Source as Source exposing (Source(..))
import Data.ColumnEditor as ColumnEditor exposing (ColumnEditor(..))
import Data.ColumnStore.AvailableSources exposing (AvailableSources)
import Data.Filter as Filter exposing (Filter, FilterAtom)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Producer.Discord as Discord
import Data.Producer.Slack.Team as SlackTeam
import Data.ProducerRegistry as ProducerRegistry
import File exposing (File)
import File.Select
import Hex
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import List.Extra
import Random
import Scroll exposing (Scroll)
import SelectArray exposing (SelectArray)
import StringExtra
import Task
import Url


type Column
    = Column ColumnRecord


type alias ColumnRecord =
    { id : Id
    , items : Scroll ColumnItem
    , filters : Array Filter
    , sources : List Source -- Unique list
    , offset : Maybe Offset
    , pinned : Bool
    , recentlyTouched : Bool -- This property may become stale, though it should have no harm
    , configOpen : Bool
    , pendingFilters : Array Filter
    , editors : SelectArray ColumnEditor
    , editorSeq : Int -- Force triggering DOM generation when incremented; workaround for https://github.com/elm/html/issues/55
    , userActionOnEditor : ColumnEditor.UserAction
    }


type alias Id =
    Id.Id String Column


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
encode (Column c) =
    E.object
        [ ( "id", Id.encode E.string c.id )
        , ( "items", Scroll.encode encodeColumnItem c.items )
        , ( "filters", E.array Filter.encode c.filters )
        , ( "sources", E.list Source.encode c.sources )
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


decoder : Int -> AvailableSources -> Decoder ( Column, Cmd Msg )
decoder clientHeight availableSources =
    let
        scrollDecoder id =
            Scroll.decoder (scrollInitOptions id clientHeight) columnItemDecoder
    in
    D.do (D.field "id" (Id.decoder D.string)) <|
        \id ->
            D.do (D.field "items" (scrollDecoder id)) <|
                \( items, sCmd ) ->
                    D.do (D.field "filters" (D.array Filter.decoder)) <|
                        \filters ->
                            new id items
                                |> setFilters filters
                                |> setPendingFilters filters
                                |> setEditors (ColumnEditor.filtersToEditors filters)
                                |> D.succeed
                                |> D.map2 setOffset (D.maybeField "offset" offsetDecoder)
                                |> D.map2 setPinned (D.field "pinned" D.bool)
                                |> D.map2 setSources (sourcesDecoder availableSources)
                                |> D.map (\c -> ( c, Cmd.map ScrollMsg sCmd ))


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


sourcesDecoder : AvailableSources -> Decoder (List Source)
sourcesDecoder { slackConvos } =
    let
        convertFromFilter filter acc =
            Filter.foldl convertFilterAtom acc filter

        convertFilterAtom fa acc =
            case fa of
                Filter.OfDiscordChannel id ->
                    DiscordChannel id :: acc

                Filter.OfSlackConversation convoId ->
                    case List.Extra.find (\c -> c.id == convoId) slackConvos of
                        Just convo ->
                            SlackConvo (SlackTeam.getId convo.team) convoId :: acc

                        Nothing ->
                            acc

                _ ->
                    acc
    in
    D.oneOf
        [ D.field "sources" (D.list Source.decoder)
        , D.field "filters" (D.array Filter.decoder)
            |> D.map (Array.foldr convertFromFilter [])
        ]


scrollInitOptions : Id -> Int -> Scroll.InitOptions
scrollInitOptions id clientHeight =
    let
        opts =
            Scroll.defaultOptions
                { id = "scroll-" ++ Id.to id
                , boundingHeight = boundingHeight clientHeight
                , minimumItemHeight = minimumItemHeight
                }
    in
    { opts | limit = columnItemLimit, baseRatio = baseRatio, tierRatio = tierRatio }


boundingHeight : Int -> Int
boundingHeight clientHeight =
    clientHeight - columnHeaderHeight


columnHeaderHeight : Int
columnHeaderHeight =
    -- Depends on View. Basically, paddingY (5px) x 2 + header icons' height (30px)
    40


minimumItemHeight : Int
minimumItemHeight =
    -- Depends on View. itemGroupPaddingY * 2 + minGroupContentsHeight
    50


columnItemLimit : Int
columnItemLimit =
    2000


baseRatio : Float
baseRatio =
    2.0


tierRatio : Float
tierRatio =
    4.0


new : Id -> Scroll ColumnItem -> Column
new id items =
    Column
        { id = id
        , items = items
        , filters = Array.empty
        , sources = []
        , offset = Nothing
        , pinned = False
        , recentlyTouched = False
        , configOpen = False
        , pendingFilters = Array.empty
        , editors = ColumnEditor.defaultEditors
        , editorSeq = 0
        , userActionOnEditor = ColumnEditor.OutOfFocus
        }


generator : Int -> Random.Generator (List ColumnItem) -> Random.Generator Column
generator clientHeight itemsGenerator =
    let
        withId id =
            Random.map (new id) <|
                Random.map (Scroll.initWith (scrollInitOptions id clientHeight)) itemsGenerator
    in
    Random.andThen withId (Random.map Id.from idGenerator)


emptyGenerator : Int -> Random.Generator Column
emptyGenerator clientHeight =
    Random.map (List.singleton << systemMessage "New column created! Let's configure filters above!") idGenerator
        |> generator clientHeight
        |> Random.map (setConfigOpen True)
        |> Random.map (setRecentlyTouched True)


systemMessage : String -> String -> ColumnItem
systemMessage message id =
    SystemMessage { id = id, message = message, mediaMaybe = Nothing }


welcomeGenerator : Int -> Random.Generator Column
welcomeGenerator clientHeight =
    let
        generateItemsFromIds =
            Random.constant
                << List.map2 (<|)
                    [ welcomeItem
                    , systemMessage "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
                    ]
    in
    Random.andThen generateItemsFromIds (Random.list 2 idGenerator)
        |> generator clientHeight
        |> Random.map (setConfigOpen True)
        |> Random.map (setRecentlyTouched True)


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


simpleGenerator : Int -> FilterAtom -> Random.Generator Column
simpleGenerator clientHeight fa =
    let
        filters =
            Array.fromList [ Filter.Singular fa ]
    in
    generator clientHeight (Random.constant [])
        |> Random.map (setFilters filters)
        |> Random.map (setEditors (ColumnEditor.filtersToEditors filters))
        |> Random.map (setRecentlyTouched True)


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
    | AddSource Source
    | DelSource Source
    | SelectEditor Int
    | EditorInteracted ColumnEditor.UserAction
    | EditorInput String
    | EditorReset
    | EditorSubmit
    | EditorFileRequest (List String)
    | EditorFileSelected File
    | EditorFileLoaded ( File, String )
    | EditorFileDiscard
    | ScanBroker ScanOptions
    | ScrollMsg Scroll.Msg


type alias ScanOptions =
    { broker : Broker Item
    , maxCount : Int
    , clientHeight : Int
    , catchUp : Bool
    }


type alias PostProcess =
    { cmd : Cmd Msg
    , persist : Bool
    , catchUpId : Maybe Id
    , position : Position
    , producerMsg : Maybe ProducerRegistry.Msg
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
    }


update : Bool -> Msg -> Column -> ( Column, PostProcess )
update isVisible msg (Column c) =
    case msg of
        ToggleConfig open ->
            pure (Column { c | configOpen = open, pendingFilters = c.filters })

        Pin pinned ->
            ( Column { c | pinned = pinned, recentlyTouched = True }
            , { postProcess | persist = True, position = Auto }
            )

        Show ->
            let
                ( items, sCmd ) =
                    Scroll.update Scroll.Reveal c.items
            in
            ( Column { c | items = items, recentlyTouched = True }
            , { postProcess | cmd = Cmd.map ScrollMsg sCmd, persist = True, position = Bump }
            )

        Calm ->
            pure (Column { c | recentlyTouched = False })

        AddFilter filter ->
            pure (Column { c | pendingFilters = Array.push filter c.pendingFilters })

        DelFilter index ->
            pure (Column { c | pendingFilters = Array.removeAt index c.pendingFilters })

        AddFilterAtom { filterIndex, atom } ->
            pure (Column { c | pendingFilters = Array.update filterIndex (Filter.append atom) c.pendingFilters })

        SetFilterAtom { filterIndex, atomIndex, atom } ->
            pure (Column { c | pendingFilters = Array.update filterIndex (Filter.setAt atomIndex atom) c.pendingFilters })

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
            pure (Column { c | pendingFilters = atomOrFilterDeleted })

        ConfirmFilter ->
            ( Column { c | filters = c.pendingFilters, offset = Nothing, items = Scroll.clear c.items, configOpen = False }
            , { postProcess | persist = True, catchUpId = Just c.id }
            )

        AddSource source ->
            let
                uniqueAdd x list =
                    if List.member x list then
                        -- Not likely gonna happen, but check anyway
                        list

                    else
                        x :: list
            in
            ( Column { c | sources = uniqueAdd source c.sources }, { postProcess | persist = True } )

        DelSource source ->
            ( Column { c | sources = List.Extra.remove source c.sources }, { postProcess | persist = True } )

        SelectEditor index ->
            pure (Column { c | editors = SelectArray.selectAt index c.editors, editorSeq = c.editorSeq + 1 })

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
            ( Column { c | userActionOnEditor = action }, { postProcess | cmd = cmd } )

        EditorInput input ->
            pure (Column { c | editors = SelectArray.updateSelected (ColumnEditor.updateBuffer input) c.editors })

        EditorReset ->
            pure (Column { c | editors = SelectArray.updateSelected ColumnEditor.reset c.editors, editorSeq = c.editorSeq + 1 })

        EditorSubmit ->
            editorSubmit (Column c)

        EditorFileRequest mimeTypes ->
            ( Column c, { postProcess | cmd = File.Select.file mimeTypes EditorFileSelected } )

        EditorFileSelected file ->
            ( Column { c | userActionOnEditor = ColumnEditor.Authoring }
            , { postProcess | cmd = Task.perform EditorFileLoaded (Task.map (Tuple.pair file) (File.toUrl file)) }
            )

        EditorFileLoaded fileTuple ->
            pure (Column { c | editors = SelectArray.updateSelected (ColumnEditor.updateFile (Just fileTuple)) c.editors })

        EditorFileDiscard ->
            pure (Column { c | editors = SelectArray.updateSelected (ColumnEditor.updateFile Nothing) c.editors })

        ScanBroker opts ->
            scanBroker isVisible opts (Column c)

        ScrollMsg sMsg ->
            if isVisible then
                let
                    ( items, sCmd ) =
                        Scroll.update sMsg c.items
                in
                ( Column { c | items = items }, { postProcess | cmd = Cmd.map ScrollMsg sCmd } )

            else
                pure (Column c)


pure : Column -> ( Column, PostProcess )
pure c =
    ( c, postProcess )


scanBroker : Bool -> ScanOptions -> Column -> ( Column, PostProcess )
scanBroker isVisible { broker, maxCount, clientHeight, catchUp } (Column c_) =
    let
        adjustOpts =
            { isVisible = isVisible, clientHeight = clientHeight, hasNewItem = False }
    in
    case ItemBroker.bulkRead maxCount c_.offset broker of
        [] ->
            adjustScroll adjustOpts ( Column c_, postProcess )

        (( _, newOffset ) :: _) as items ->
            let
                c =
                    { c_ | offset = Just newOffset }

                ppBase =
                    { postProcess | persist = True }
            in
            case ( catchUp, List.filterMap (filterBySources c.sources) items ) of
                ( True, [] ) ->
                    adjustScroll adjustOpts ( Column c, { ppBase | catchUpId = Just c.id } )

                ( True, newItems ) ->
                    adjustScroll { adjustOpts | hasNewItem = True } <|
                        -- Do not bump, nor flash Column during catchUp
                        ( Column { c | items = Scroll.prependList newItems c.items }
                        , { ppBase | catchUpId = Just c.id }
                        )

                ( False, [] ) ->
                    adjustScroll adjustOpts ( Column c, ppBase )

                ( False, newItems ) ->
                    adjustScroll { adjustOpts | hasNewItem = True } <|
                        ( Column { c | items = Scroll.prependList newItems c.items, recentlyTouched = True }
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
adjustScroll { isVisible, clientHeight, hasNewItem } ( Column c, pp ) =
    let
        ( items, sCmd ) =
            if hasNewItem then
                Scroll.update Scroll.NewItem c.items

            else if isVisible then
                Scroll.update (Scroll.AdjustReq (boundingHeight clientHeight)) c.items

            else
                ( c.items, Cmd.none )
    in
    ( Column { c | items = items }, { pp | cmd = Cmd.map ScrollMsg sCmd } )


filterBySources : List Source -> ( Item, Offset ) -> Maybe ColumnItem
filterBySources sources ( item, offset ) =
    if List.any (Item.isFromSource item) sources then
        -- List.any return False when sources are empty; not allowing "pass all"
        Just (Product offset item)

    else
        Nothing


editorSubmit : Column -> ( Column, PostProcess )
editorSubmit (Column c) =
    case SelectArray.selected c.editors of
        DiscordMessageEditor { channelId, buffer, file } ->
            if String.isEmpty buffer && file == Nothing then
                pure (Column c)

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
                ( Column
                    { c
                        | editors = SelectArray.updateSelected ColumnEditor.reset c.editors
                        , editorSeq = c.editorSeq + 1
                        , userActionOnEditor = ColumnEditor.OutOfFocus
                    }
                , { postProcess | catchUpId = Just c.id, producerMsg = Just postMsg }
                )

        LocalMessageEditor buffer ->
            if String.isEmpty buffer then
                pure (Column c)

            else
                saveLocalMessage buffer (Column c)


saveLocalMessage : String -> Column -> ( Column, PostProcess )
saveLocalMessage buffer (Column c) =
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
    ( Column
        { c
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


editorId : Id -> String
editorId id =
    "newMessageEditor_" ++ Id.to id



-- Accessors


getId : Column -> Id
getId (Column c) =
    c.id


getItems : Column -> Scroll ColumnItem
getItems (Column c) =
    c.items


getFilters : Column -> Array Filter
getFilters (Column c) =
    c.filters


getSources : Column -> List Source
getSources (Column c) =
    c.sources


getOffset : Column -> Maybe Offset
getOffset (Column c) =
    c.offset


getPinned : Column -> Bool
getPinned (Column c) =
    c.pinned


getRecentlyTouched : Column -> Bool
getRecentlyTouched (Column c) =
    c.recentlyTouched


getConfigOpen : Column -> Bool
getConfigOpen (Column c) =
    c.configOpen


getPendingFilters : Column -> Array Filter
getPendingFilters (Column c) =
    c.pendingFilters


getEditors : Column -> SelectArray ColumnEditor
getEditors (Column c) =
    c.editors


getEditorSeq : Column -> Int
getEditorSeq (Column c) =
    c.editorSeq


getUserActionOnEditor : Column -> ColumnEditor.UserAction
getUserActionOnEditor (Column c) =
    c.userActionOnEditor


setId : Id -> Column -> Column
setId val (Column c) =
    Column { c | id = val }


setItems : Scroll ColumnItem -> Column -> Column
setItems val (Column c) =
    Column { c | items = val }


setFilters : Array Filter -> Column -> Column
setFilters val (Column c) =
    Column { c | filters = val }


setSources : List Source -> Column -> Column
setSources val (Column c) =
    Column { c | sources = val }


setOffset : Maybe Offset -> Column -> Column
setOffset val (Column c) =
    Column { c | offset = val }


setPinned : Bool -> Column -> Column
setPinned val (Column c) =
    Column { c | pinned = val }


setRecentlyTouched : Bool -> Column -> Column
setRecentlyTouched val (Column c) =
    Column { c | recentlyTouched = val }


setConfigOpen : Bool -> Column -> Column
setConfigOpen val (Column c) =
    Column { c | configOpen = val }


setPendingFilters : Array Filter -> Column -> Column
setPendingFilters val (Column c) =
    Column { c | pendingFilters = val }


setEditors : SelectArray ColumnEditor -> Column -> Column
setEditors val (Column c) =
    Column { c | editors = val }


setEditorSeq : Int -> Column -> Column
setEditorSeq val (Column c) =
    Column { c | editorSeq = val }


setUserActionOnEditor : ColumnEditor.UserAction -> Column -> Column
setUserActionOnEditor val (Column c) =
    Column { c | userActionOnEditor = val }
