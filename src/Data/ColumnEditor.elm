module Data.ColumnEditor exposing
    ( ColumnEditor(..), defaultEditors
    , filtersToEditors, updateBuffer, reset, updateFile
    )

{-| Editor data for Columns.

@docs ColumnEditor, defaultEditors
@docs filtersToEditors, updateBuffer, reset, updateFile

-}

import Array exposing (Array)
import Data.Filter as Filter exposing (Filter, FilterAtom(..))
import File exposing (File)
import SelectArray exposing (SelectArray)


type ColumnEditor
    = DiscordMessageEditor DiscordOpts
    | LocalMessageEditor String


type alias DiscordOpts =
    { channelId : String
    , buffer : String
    , file : Maybe ( File, String )
    }


localMessageEditor : ColumnEditor
localMessageEditor =
    LocalMessageEditor ""


filtersToEditors : Array Filter -> SelectArray ColumnEditor
filtersToEditors filters =
    let
        leftArrayReducer f accList =
            Filter.foldl leftFilterReducer accList f

        leftFilterReducer fa accList =
            case fa of
                OfDiscordChannel cId ->
                    DiscordMessageEditor (DiscordOpts cId "" Nothing) :: accList

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
        DiscordMessageEditor opts ->
            DiscordMessageEditor { opts | buffer = input }

        LocalMessageEditor _ ->
            LocalMessageEditor input


reset : ColumnEditor -> ColumnEditor
reset ce =
    case ce of
        DiscordMessageEditor opts ->
            DiscordMessageEditor { opts | buffer = "", file = Nothing }

        LocalMessageEditor _ ->
            LocalMessageEditor ""


updateFile : Maybe ( File, String ) -> ColumnEditor -> ColumnEditor
updateFile file ce =
    case ce of
        DiscordMessageEditor opts ->
            DiscordMessageEditor { opts | file = file }

        LocalMessageEditor _ ->
            ce
