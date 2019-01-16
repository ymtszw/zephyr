module Data.ColumnEditor exposing
    ( ColumnEditor(..), CommonEditorOpts, defaultEditors
    , filtersToEditors, updateBuffer, reset, updateFile
    )

{-| Editor data for Columns.

@docs ColumnEditor, CommonEditorOpts, defaultEditors
@docs filtersToEditors, updateBuffer, reset, updateFile

-}

import Array exposing (Array)
import Data.Filter as Filter exposing (Filter, FilterAtom(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Producer.Discord as Discord
import File exposing (File)
import SelectArray exposing (SelectArray)


type ColumnEditor
    = DiscordMessageEditor DiscordOpts CommonEditorOpts
    | LocalMessageEditor CommonEditorOpts


type alias CommonEditorOpts =
    { buffer : String }


type alias DiscordOpts =
    { channelId : String
    , file : Maybe ( File, String )
    }


localMessageEditor : ColumnEditor
localMessageEditor =
    LocalMessageEditor defaultOpts


defaultOpts : CommonEditorOpts
defaultOpts =
    { buffer = "" }


filtersToEditors : Array Filter -> SelectArray ColumnEditor
filtersToEditors filters =
    let
        leftArrayReducer f accList =
            Filter.foldl leftFilterReducer accList f

        leftFilterReducer fa accList =
            case fa of
                OfDiscordChannel cId ->
                    DiscordMessageEditor (DiscordOpts cId Nothing) defaultOpts :: accList

                _ ->
                    accList
    in
    case filters |> Array.foldl leftArrayReducer [] |> List.reverse of
        [] ->
            SelectArray.singleton localMessageEditor

        e :: es ->
            SelectArray.fromLists [] e (es ++ [ localMessageEditor ])


defaultEditors : SelectArray ColumnEditor
defaultEditors =
    SelectArray.singleton localMessageEditor


updateBuffer : String -> ColumnEditor -> ColumnEditor
updateBuffer input ce =
    case ce of
        DiscordMessageEditor dOpts opts ->
            DiscordMessageEditor dOpts { opts | buffer = input }

        LocalMessageEditor opts ->
            LocalMessageEditor { opts | buffer = input }


reset : ColumnEditor -> ColumnEditor
reset ce =
    case ce of
        DiscordMessageEditor dOpts opts ->
            DiscordMessageEditor
                { dOpts | file = Nothing }
                { opts | buffer = "" }

        LocalMessageEditor opts ->
            LocalMessageEditor
                { opts | buffer = "" }


updateFile : Maybe ( File, String ) -> ColumnEditor -> ColumnEditor
updateFile file ce =
    case ce of
        DiscordMessageEditor dOpts opts ->
            DiscordMessageEditor { dOpts | file = file } opts

        LocalMessageEditor opts ->
            LocalMessageEditor opts