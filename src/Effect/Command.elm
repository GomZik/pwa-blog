module Effect.Command exposing (..)

import Effect.Command.Internal as I


type alias Command msg =
  I.Command msg


none : Command msg
none = I.Batch []

batch : List ( Command msg ) -> Command msg
batch = I.Batch

cmd : Cmd msg -> Command msg
cmd = I.Platform
