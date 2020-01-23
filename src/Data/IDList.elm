module Data.IDList exposing (..)


type IDList id entity = IDList
  { currentId : id
  , makeNextId : ( id -> id )
  , entities : List entity
  }

empty : id -> ( id -> id ) -> IDList id entity
empty id make =
  IDList
    { currentId = id
    , makeNextId = make
    , entities = []
    }

add : ( id -> entity ) -> IDList id entity -> ( entity, IDList id entity )
add create ( IDList data ) =
  let
    nextId = data.makeNextId data.currentId
    entity = create nextId

    newList = data.entities ++ [ entity ]
  in
    ( entity
    , IDList
      { data | currentId = nextId
             , entities = newList
      }
    )

push : ( entity -> id ) -> ( id -> id -> Order ) -> entity -> IDList id entity -> IDList id entity
push getter cmp data ( IDList lst ) =
  let
    dataId = getter data
    ord = cmp lst.currentId dataId
    currentId = case ord of
      GT -> lst.currentId
      _ -> dataId
  in
    IDList
      { lst | currentId = currentId
            , entities = lst.entities ++ [ data ]
      }

map : ( entity -> b ) -> IDList id entity -> List b
map fn ( IDList lst ) =
  lst.entities
    |> List.map fn
