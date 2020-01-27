module Main exposing ( main )

import Effect.Program exposing ( Program, program )
import Effect.Command as Command exposing ( Command )
import Effect.Storage as Storage

import Data.Message as Message exposing ( Message )
import Data.IDList as IDList exposing ( IDList )
import Data.NonEmptyList as NonEmptyList exposing ( NonEmptyList )

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
  , here : Maybe Time.Zone
  }

type Msg
  = GotStorageData Value
  | OnInput String
  | Submit
  | GotMessageTime String Time.Posix
  | GotZone Time.Zone

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
    , here = Nothing
    }
  , Command.batch
    [ Storage.load GotStorageData "messages"
    , Command.cmd <| Task.perform GotZone Time.here
    ]
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
        Ok ( Just msgs ) ->
          ( { model | messages = msgs
                        |> List.foldl
                          ( IDList.push Message.id compare )
                          model.messages
            }
          , Command.none
          )

        _ ->
          ( model, Command.none )

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

    GotZone zone ->
      ( { model | here = Just zone }
      , Command.none
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


timeToText : Maybe Time.Zone -> Time.Posix -> String
timeToText mbhere t =
  mbhere
    |> Maybe.map (\here ->
      let
        hours = Time.toHour here t
        minutes = Time.toMinute here t
      in
        [ String.fromInt hours
            |> String.padLeft 2 '0'
        , String.fromInt minutes
            |> String.padLeft 2 '0'
        ] |> String.join ":"
    )
    |> Maybe.withDefault ""

viewMessage : Maybe Time.Zone -> Message -> Html Msg
viewMessage here msg =
  div [ class "message" ]
  [ p [ class "text" ] [ text <| Message.text msg ]
  , p [ class "time" ] [ text <| timeToText here <| Message.created msg ]
  ]


viewDateDelimiter : Maybe Time.Zone -> Time.Posix -> Html Msg
viewDateDelimiter mbHere t =
  text ""

viewMessageGroup : Maybe Time.Zone -> MessageGroup -> Html Msg
viewMessageGroup here grp =
  case grp of
    Messages lst ->
      div [ class "message-group" ]
        <| List.map ( viewMessage here ) ( NonEmptyList.toList lst )
    DateDelimiter t ->
      viewDateDelimiter here t


type MessageGroup
  = Messages ( NonEmptyList Message )
  | DateDelimiter Time.Posix


type DateDiff
  = LessThanHour
  | LessThanDay
  | GreaterThanDay

msgDateDiff : Time.Zone -> Message -> Message -> DateDiff
msgDateDiff zone msg1 msg2 =
  let
    t1 = Message.created msg1
    t2 = Message.created msg2
    sameYear = Time.toYear zone t1 == Time.toYear zone t2
    sameMonth = Time.toMonth zone t1 == Time.toMonth zone t2
    sameDay = Time.toDay zone t1 == Time.toDay zone t2

    oneDay = sameYear && sameMonth && sameDay

    millsDiff = Time.posixToMillis t1 - Time.posixToMillis t2
  in
    case ( oneDay, millsDiff < 3600000 ) of
      ( False, _ ) -> GreaterThanDay
      ( True, True ) -> LessThanHour
      _ -> LessThanDay


groupMessages : Maybe Time.Zone -> List Message -> List MessageGroup
groupMessages mbZone messages =
  let
    helper msgs mbPrev acc =
      case msgs of
        [] ->
          case mbPrev of
            Nothing -> acc
              |> List.reverse

            Just prev -> ( prev :: acc )
              |> List.reverse

        x :: xs ->
          case mbPrev of
            Nothing ->
              helper xs ( Just <| Messages <| NonEmptyList.singleton x ) acc

            Just ( ( DateDelimiter t ) as p ) ->
              helper xs ( Just <| Messages <| NonEmptyList.singleton x ) ( p :: acc )

            Just ( ( Messages lst ) as ms ) ->
              case mbZone of
                Just zone ->
                  case msgDateDiff zone x ( NonEmptyList.last lst ) of
                    LessThanHour ->
                      helper xs ( Just <| Messages <| NonEmptyList.append x lst ) acc

                    LessThanDay ->
                      helper xs ( Just <| Messages <| NonEmptyList.singleton x ) ( ms :: acc )

                    GreaterThanDay ->
                      helper ( x :: xs ) ( Just <| DateDelimiter <| Message.created x ) acc

                Nothing ->
                  helper xs ( Just <| Messages <| NonEmptyList.append x lst ) acc


  in
    helper messages Nothing []

viewBody : Model -> Html Msg
viewBody model =
  div [ class "messages" ]
    ( model.messages
      |> IDList.toList
      |> groupMessages model.here
      |> List.map ( viewMessageGroup model.here )
    )


viewInput : Model -> Html Msg
viewInput model =
  div [ class "input" ]
    [ Html.form [ onSubmit Submit ]
      [ input [ type_ "text", value model.inputText, onInput OnInput ] []
      , button [ type_ "submit" ] [ text "submit" ]
      ]
    ]
