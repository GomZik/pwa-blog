module Time.Extra exposing (..)

import Time

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD exposing ( Decoder )

decoder : Decoder Time.Posix
decoder =
  JD.int
    |> JD.map Time.millisToPosix

encode : Time.Posix -> Value
encode t =
  t
    |> Time.posixToMillis
    |> JE.int
