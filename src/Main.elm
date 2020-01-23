module Main exposing ( main )

import Effect.Program exposing ( Program, program )
import Effect.Command as Command exposing ( Command )
import Effect.Storage as Storage

import Data.Message as Message exposing ( Message )
import Data.IDList as IDList exposing ( IDList )

import Browser

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD

import Time

import Task

type alias Model =
  { messages : IDList Int Message
  , inputText : String
  }

type Msg
  = GotStorageData Value
  | OnInput String
  | Submit
  | GotMessageTime String Time.Posix

main : Program () Model Msg
main =
  program
    { init = init
    , update = update
    , view = viewDocument
    }

init : () -> ( Model, Command Msg )
init _ =
  ( { messages = IDList.empty 0 ((+) 1)
    , inputText = ""
    }
  , Storage.load GotStorageData "messages"
  )


storeMessages : IDList Int Message -> Command Msg
storeMessages lst =
  lst
    |> IDList.map Message.encode
    |> JE.list identity
    |> Storage.save "messages"


update : Msg -> Model -> ( Model, Command Msg )
update msg model =
  -- case Debug.log "Main.update" msg of
  case msg of
    GotStorageData val ->
      case JD.decodeValue ( JD.maybe <| JD.list Message.decoder ) val of
        Err err ->
          -- Message storage was corrupted?
          -- let
          --   _ = Debug.log "decode error" err
          -- in
          ( model, Command.none )

        Ok res ->
          -- case Debug.log "messages" res of
          case res of
            Nothing ->
              ( model, Command.none )
            Just msgs ->
              let
                step messages lst =
                  case messages of
                    [] -> lst
                    x :: xs ->
                      step xs
                        <| IDList.push Message.id compare x lst
              in
                ( { model | messages = step msgs model.messages }
                , Command.none
                )
    OnInput str ->
      ( { model | inputText = str }, Command.none )
    Submit ->
      case model.inputText of
        "" -> ( model, Command.none )
        str ->
          ( { model | inputText = "" }
          , Task.perform ( GotMessageTime str ) Time.now
            |> Command.cmd
          )
    GotMessageTime msgText msgTime ->
      let
        ( _, messages ) = model.messages
          |> IDList.add (\id -> Message.new id msgTime msgText )
      in
        ( { model | messages = messages }
        , storeMessages messages
        )


viewDocument : Model -> Browser.Document Msg
viewDocument model =
  { title = "PWA Blog"
  , body =
    [ viewHeader model
    , viewBody model
    , viewInput model
    ]
  }

viewHeader : Model -> Html Msg
viewHeader model =
  div [ class "header" ] [ text "header" ]


viewMessage : Message -> Html Msg
viewMessage msg =
  div [ class "message" ] [ text <| Message.text msg ]

viewBody : Model -> Html Msg
viewBody model =
  div [ class "messages" ]
    ( model.messages
      |> IDList.map viewMessage
    )


viewInput : Model -> Html Msg
viewInput model =
  div [ class "input" ]
    [ Html.form [ onSubmit Submit ]
      [ input [ type_ "text", value model.inputText, onInput OnInput ] []
      , button [ type_ "submit" ] [ text "submit" ]
      ]
    ]
