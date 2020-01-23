module Effect.Command.Internal exposing (..)

import Json.Encode as JE exposing ( Value )

type Command msg
  = Batch ( List ( Command msg ) )
  | Platform ( Cmd msg )
  | LoadStorage ( Value -> msg ) String
