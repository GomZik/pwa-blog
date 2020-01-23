port module Effect.Storage.Internal exposing (..)

import Json.Encode exposing ( Value )

port loadDataRequest : Value -> Cmd msg
port loadDataResponse : ( Value -> msg ) -> Sub msg
