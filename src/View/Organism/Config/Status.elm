module View.Organism.Config.Status exposing (Props, render)

import Html exposing (Html, div)
import StringExtra
import View.Atom.Layout exposing (flexColumn, padding5, spacingColumn5)
import View.Atom.Typography exposing (t)


type alias Props =
    { itemBrokerCapacity : Int
    , columnItemLimit : Int
    , numColumns : Int
    , numVisible : Int
    , numPinned : Int
    , clientHeight : Int
    , clientWidth : Int
    , serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    }


render : Props -> Html msg
render p =
    div [ flexColumn, padding5, spacingColumn5 ] <|
        List.map (div [] << List.map t << List.intersperse " - ")
            [ [ "Local message buffer capacity", StringExtra.punctuateNumber p.itemBrokerCapacity ]
            , [ "Maximum messages per column", StringExtra.punctuateNumber p.columnItemLimit ]
            , [ "Number of columns", StringExtra.punctuateNumber p.numColumns ]
            , [ "* Visible columns", StringExtra.punctuateNumber p.numVisible ]
            , [ "* Pinned columns", StringExtra.punctuateNumber p.numPinned ]
            , [ "* Shadow columns", StringExtra.punctuateNumber (p.numColumns - p.numVisible) ]
            , [ "ClientHeight", StringExtra.punctuateNumber p.clientHeight ]
            , [ "ClientWidth", StringExtra.punctuateNumber p.clientWidth ]
            , [ "ServiceWorker"
              , if p.serviceWorkerAvailable then
                    "Registered"

                else
                    "Not available"
              ]
            , [ "IndexedDB"
              , if p.indexedDBAvailable then
                    "Used"

                else
                    "Not available"
              ]
            ]
