module Data.Message exposing (..)

import Time
import Time.Extra as Time

import Json.Encode as JE exposing ( Value )
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

decoder : Decoder Message
decoder =
  JD.map3 Internals
    ( JD.field "id" JD.int )
    ( JD.field "created" Time.decoder )
    ( JD.field "text" JD.string )
  |> JD.map M


new : Int -> Time.Posix -> String -> Message
new a b c =
  Internals a b c |> M


encode : Message -> Value
encode ( M data ) =
  JE.object
    [ ( "id", JE.int data.id )
    , ( "created", Time.encode data.created )
    , ( "text", JE.string data.text )
    ]
