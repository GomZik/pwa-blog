module Scroll exposing (..)

import Browser.Dom

import Ease

import Task exposing ( Task )

type alias Config =
  { time : Int
  , easing : Ease.Easing
  }

defaultConfig : Config
defaultConfig =
  { time = 300
  , easing = Ease.outQuint
  }

buildFrames : Config -> Browser.Dom.Viewport -> Task Browser.Dom.Error ()
buildFrames cfg vp =
  let
    diff = abs <| vp.scene.height - vp.viewport.y - vp.viewport.height
    frames = max 1 <| round <| toFloat cfg.time / 16.6667
    framesFloat = toFloat frames
    weights = List.map (\i -> cfg.easing ( toFloat i / framesFloat )) ( List.range 0 frames )

  in
    List.map (\w -> Browser.Dom.setViewport 0 <| vp.viewport.y + w * diff ) weights
      |> Task.sequence
      |> Task.map ( always () )


scrollBottom : Config -> Task Browser.Dom.Error ()
scrollBottom cfg =
  Browser.Dom.getViewport
    |> Task.andThen ( buildFrames cfg )
