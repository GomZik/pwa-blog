port module Effect.Storage exposing
  ( save, load )

import Effect.Command.Internal exposing ( Command(..) )

import Json.Encode as JE exposing ( Value )

port setItem : Value -> Cmd msg

save : String -> Value -> Command msg
save name val =
  JE.object
    [ ( "item", JE.string name )
    , ( "data", val )
    ]
  |> setItem
  |> Platform


load : ( Value -> msg ) -> String -> Command msg
load = LoadStorage
