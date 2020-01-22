module Main exposing ( main )

import Browser

import Html exposing (..)

type Model = Model

type Msg = NoOp

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = viewDocument
    , subscriptions = always Sub.none
    }

init : () -> ( Model, Cmd Msg )
init _ =
  ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
  ( model, Cmd.none )


viewDocument : Model -> Browser.Document Msg
viewDocument model =
  { title = "PWA Blog"
  , body = [ text "Hello, world" ]
  }
