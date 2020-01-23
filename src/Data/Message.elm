module Data.Message exposing (..)

import Time

import Json.Decode as JD exposing ( Decoder )

type alias Internals =
  { id : Int
  , created : Time.Posix
  , text : String
  }

type Message = M Internals

id : Message -> Int
id ( M data ) = data.id


created : Message -> Time.Posix
created ( M data ) = data.created


text : Message -> String
text ( M data ) = data.text


timeDecoder : Decoder Time.Posix
timeDecoder =
  JD.int
    |> JD.map Time.millisToPosix

decoder : Decoder Message
decoder =
  JD.map3 Internals
    ( JD.field "id" JD.int )
    ( JD.field "created" timeDecoder )
    ( JD.field "text" JD.string )
  |> JD.map M
