module Data.ColumnEditor exposing
    ( ColumnEditor(..), UserAction(..), defaultEditors
    , filtersToEditors, getBuffer, updateBuffer, reset, updateFile
    )

{-| Editor data for Columns.

@docs ColumnEditor, UserAction, defaultEditors
@docs filtersToEditors, getBuffer, updateBuffer, reset, updateFile

-}

import Array exposing (Array)
import Data.Filter as Filter exposing (Filter, FilterAtom(..))
import Data.Producer.Discord.Channel as DiscordChannel
import File exposing (File)
import SelectArray exposing (SelectArray)


type ColumnEditor
    = DiscordMessageEditor DiscordOpts
    | LocalMessageEditor String


type alias DiscordOpts =
    { channelId : DiscordChannel.Id
    , buffer : String
    , file : Maybe ( File, String )
    }


{-| Indicates current user's action against the editor.

HoveringFiles enables dynamic styling upon file hover.

-}
type UserAction
    = OutOfFocus
    | Authoring
    | HoveringFiles
    | HoveringNonFile


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


getBuffer : ColumnEditor -> String
getBuffer editor =
    case editor of
        DiscordMessageEditor opts ->
            opts.buffer

        LocalMessageEditor buffer ->
            buffer


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
