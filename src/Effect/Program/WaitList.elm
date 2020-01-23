module Effect.Program.WaitList exposing (..)


import Json.Encode as JE exposing ( Value )
import Json.Decode as JD


type Key a = Key Int

type WaitList a = WaitList ( Key a ) ( List ( Key a, a ) )

tupleSwap : ( a, b ) -> ( b, a )
tupleSwap ( a, b ) = ( b, a )

keyToValue : Key a -> Value
keyToValue ( Key k ) =
  JE.int k

keyDecoder : JD.Decoder ( Key a )
keyDecoder =
  JD.int
    |> JD.map Key

empty : WaitList a
empty = WaitList ( Key 0 ) []

add : a -> WaitList a -> ( Key a, WaitList a )
add itm ( WaitList ( Key nextID ) old ) =
  ( Key nextID, WaitList ( Key <| nextID + 1 ) ( ( Key nextID, itm ) :: old ) )

pop : Key a -> WaitList a -> Maybe ( a, WaitList a )
pop ( Key id ) ( WaitList key itms ) =
  let
    ( newItms, itmList ) = itms
      |> List.partition (\( ( Key itmId ), _ ) -> itmId /= id )
  in
    itmList
      |> List.head
      |> Maybe.map ( \( _, itm ) -> itm)
      |> Maybe.map ( Tuple.pair ( WaitList key newItms ) )
      |> Maybe.map tupleSwap
