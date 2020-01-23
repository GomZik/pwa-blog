module Main exposing ( main )

import Effect.Program exposing ( Program, program )
import Effect.Command as Command exposing ( Command )
import Effect.Storage as Storage

import Data.Message as Message exposing ( Message )

import Browser

import Html exposing (..)

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD

type alias Model =
  { messages : List Message
  }

type Msg
  = GotStorageData Value

main : Program () Model Msg
main =
  program
    { init = init
    , update = update
    , view = viewDocument
    }

init : () -> ( Model, Command Msg )
init _ =
  ( Model []
  , Storage.load GotStorageData "messages"
  )


update : Msg -> Model -> ( Model, Command Msg )
update msg model =
  -- case Debug.log "Main.update" msg of
  case msg of
    GotStorageData val ->
      case JD.decodeValue ( JD.maybe <| JD.list Message.decoder ) val of
        Err _ ->
          -- Message storage was corrupted?
          ( model, Command.none )

        Ok res ->
          -- case Debug.log "messages" res of
          case res of
            Nothing ->
              ( model, Command.none )
            Just msgs ->
              ( { model | messages = msgs }, Command.none )


viewDocument : Model -> Browser.Document Msg
viewDocument model =
  { title = "PWA Blog"
  , body = [ text "Hello, world" ]
  }
