module Data.ColumnEditor exposing
    ( ColumnEditor(..), CommonEditorOpts, defaultEditors
    , filtersToEditors, updateBuffer, focus
    )

{-| Editor data for Columns.

@docs ColumnEditor, CommonEditorOpts, defaultEditors
@docs filtersToEditors, updateBuffer, focus

-}

import Array exposing (Array)
import Data.Filter as Filter exposing (Filter, FilterAtom(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Producer.Discord as Discord
import SelectArray exposing (SelectArray)


type ColumnEditor
    = DiscordMessageEditor String CommonEditorOpts
    | LocalMessageEditor CommonEditorOpts


type alias CommonEditorOpts =
    { buffer : String, focused : Bool }


localMessageEditor : ColumnEditor
localMessageEditor =
    LocalMessageEditor defaultOpts


defaultOpts : CommonEditorOpts
defaultOpts =
    { buffer = "", focused = False }


filtersToEditors : Array Filter -> SelectArray ColumnEditor
filtersToEditors filters =
    let
        leftArrayReducer f accList =
            Filter.foldl leftFilterReducer accList f

        leftFilterReducer fa accList =
            case fa of
                OfDiscordChannel cId ->
                    DiscordMessageEditor cId defaultOpts :: accList

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
        DiscordMessageEditor cId opts ->
            DiscordMessageEditor cId { opts | buffer = input }

        LocalMessageEditor opts ->
            LocalMessageEditor { opts | buffer = input }


focus : Bool -> ColumnEditor -> ColumnEditor
focus focused ce =
    case ce of
        DiscordMessageEditor cId opts ->
            DiscordMessageEditor cId { opts | focused = focused }

        LocalMessageEditor opts ->
            LocalMessageEditor { opts | focused = focused }
