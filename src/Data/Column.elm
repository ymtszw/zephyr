module Data.Column exposing (Column, decoder, welcome)

import Data.Item as Item exposing (Item)
import Json.Decode as D exposing (Decoder)


type alias Column =
    { id : String
    , items : List Item
    }


decoder : Decoder Column
decoder =
    D.map2 Column
        (D.field "id" D.string)
        (D.field "items" (D.list Item.decoder))


welcome : String -> Column
welcome id =
    { id = id
    , items =
        [ Item.welcome
        , Item.textOnly "Text only message is also possible!"
        , Item.textOnly "Design is obviously inspired by Tweetdeck. Scrollbar style is only applied in webkit-family browsers."
        , Item.textOnly "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        ]
            |> List.repeat 2
            |> List.concat
    }