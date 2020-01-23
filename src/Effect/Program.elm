module Effect.Program exposing ( Program, program )

import Effect.Command.Internal as CmdI exposing ( Command )
import Effect.Program.WaitList as WaitList exposing ( WaitList )
import Effect.Storage.Internal as Storage

import Browser
import Html

import Json.Encode as JE exposing ( Value )
import Json.Decode as JD


type alias EffectModel msg =
  { loadStorageWaiters : WaitList ( Value -> msg )
  }

type Model model msg
  = NotInitialized ( EffectModel msg )
  | Initialized model ( EffectModel msg )

type Msg msg
  = NoOp
  | AppMsg msg
  | StorageResponse Value

type StorageError
  = JsonError JD.Error
  | CallbackNotFound

type alias Program flags model msg =
  Platform.Program flags ( Model model msg ) ( Msg msg )


runBatchCmd : List ( Command msg ) -> EffectModel msg -> List ( Cmd ( Msg msg ) ) -> ( EffectModel msg, Cmd ( Msg msg ) )
runBatchCmd cmds model cmdAcc =
  case cmds of
    [] ->
      ( model
      , cmdAcc
        |> List.reverse
        |> Cmd.batch
      )
    x :: rest ->
      let
        ( nextModel, cmd ) = mapCmd model x
      in
        runBatchCmd rest nextModel ( cmd :: cmdAcc )


mapCmd : EffectModel msg -> Command msg -> ( EffectModel msg, Cmd ( Msg msg ) )
mapCmd model cmd =
  case cmd of
    CmdI.Batch [] -> ( model, Cmd.none )
    CmdI.Batch cmds ->
      runBatchCmd cmds model []
    CmdI.Platform pcmd ->
      ( model, Cmd.map AppMsg pcmd )
    CmdI.LoadStorage cb name ->
      let
        ( id, newList ) = WaitList.add cb model.loadStorageWaiters

        idEncoded = WaitList.keyToValue id
      in
        ( { model | loadStorageWaiters = newList }
        , JE.object
          [ ( "requestId", idEncoded )
          , ( "keyName", JE.string name )
          ]
          |> Storage.loadDataRequest
        )

runCommand : Model model msg -> ( model, Command msg ) -> ( Model model msg, Cmd ( Msg msg ) )
runCommand model ( appModel, cmd ) =
  let
    effectModel = getEffectsModel model

    ( newModel, resultCmd ) = mapCmd effectModel cmd
  in
    ( Initialized appModel newModel, resultCmd )

wrapInit : ( flags -> ( model, Command msg ) ) -> flags -> ( Model model msg, Cmd ( Msg msg ) )
wrapInit appInit flags =
  appInit flags
    |> runCommand ( NotInitialized { loadStorageWaiters = WaitList.empty } )


getEffectsModel : Model model msg -> EffectModel msg
getEffectsModel model =
  case model of
    NotInitialized eff -> eff
    Initialized _ eff -> eff

processStorageResponse : Value -> EffectModel msg -> Result StorageError ( EffectModel msg, msg )
processStorageResponse val eff =
  let
    decoder =
      JD.map2 Tuple.pair
        ( JD.field "requestId" WaitList.keyDecoder )
        ( JD.field "data" JD.value )
  in
    JD.decodeValue decoder val
      |> Result.mapError JsonError
      |> Result.andThen (\(key, data) ->
        let
          found = WaitList.pop key eff.loadStorageWaiters
        in
          case found of
            Nothing -> Err CallbackNotFound
            Just ( cb, newList ) -> Ok <| ( { eff | loadStorageWaiters = newList }, cb data )
      )

wrapUpdate : ( msg -> model -> ( model, Command msg ) ) -> Msg msg -> Model model msg -> ( Model model msg, Cmd ( Msg msg ) )
wrapUpdate appUpdate msg model =
  case ( msg, model ) of
    ( NoOp, _ ) -> ( model, Cmd.none )

    ( AppMsg appMsg, Initialized appModel _ ) ->
      appUpdate appMsg appModel
        |> runCommand model

    ( StorageResponse data, _ ) ->
      let
        eff = getEffectsModel model
      in
        case ( processStorageResponse data eff, model ) of
          ( Ok ( newEff, appMsg ), Initialized appModel _ ) ->
            appUpdate appMsg appModel
              |> runCommand ( Initialized appModel newEff )
          _ -> ( model, Cmd.none )

    ( _, NotInitialized _ ) ->
      ( model, Cmd.none )


wrapView : ( model -> Browser.Document msg ) -> Model model msg -> Browser.Document ( Msg msg )
wrapView appView model =
  let
    doc = case model of
      NotInitialized _ -> { title = "loading", body = [] }
      Initialized appModel _ -> appView appModel
  in
    { title = doc.title
    , body =
      doc.body
        |> List.map ( Html.map AppMsg )
    }

program :
  { init : flags -> ( model, Command msg )
  , update : msg -> model -> ( model, Command msg )
  , view : model -> Browser.Document msg
  } -> Program flags model msg
program cfg =
  Browser.document
    { init = wrapInit cfg.init
    , update = wrapUpdate cfg.update
    , view = wrapView cfg.view
    , subscriptions = subscriptions
    }


subscriptions : Model model msg -> Sub ( Msg msg )
subscriptions model =
  Storage.loadDataResponse StorageResponse
