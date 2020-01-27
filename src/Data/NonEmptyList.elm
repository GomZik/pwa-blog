module Data.NonEmptyList exposing (..)


type NonEmptyList x = NonEmptyList x ( List x )


singleton : x -> NonEmptyList x
singleton itm =
  NonEmptyList itm []


append : x -> NonEmptyList x -> NonEmptyList x
append itm ( NonEmptyList lst list ) =
  NonEmptyList itm ( lst :: list )


toList : NonEmptyList x -> List x
toList ( NonEmptyList lst list ) =
  ( lst :: list )
    |> List.reverse

last : NonEmptyList x -> x
last ( NonEmptyList lst _ ) =
  lst
