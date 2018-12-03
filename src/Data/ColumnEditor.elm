module Data.ColumnEditor exposing (ColumnEditor(..), defaultEditors, filtersToEditors, updateBuffer)

import Array exposing (Array)
import Data.Filter as Filter exposing (Filter, FilterAtom(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Producer.Discord as Discord
import SelectArray exposing (SelectArray)


type ColumnEditor
    = DiscordMessageEditor { channelId : String, buffer : String }
    | LocalMessageEditor { buffer : String }


localMessageEditor : ColumnEditor
localMessageEditor =
    LocalMessageEditor { buffer = "" }


filtersToEditors : Array Filter -> SelectArray ColumnEditor
filtersToEditors filters =
    let
        leftArrayReducer f accList =
            Filter.foldl leftFilterReducer accList f

        leftFilterReducer fa accList =
            case fa of
                OfDiscordChannel cId ->
                    DiscordMessageEditor { channelId = cId, buffer = "" } :: accList

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

        LocalMessageEditor opts ->
            LocalMessageEditor { opts | buffer = input }
