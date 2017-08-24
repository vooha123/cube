module Main where

import Prelude
import Data.Maybe (Maybe(..))

import Control.Monad.Eff (Eff)
import Math (pi, cos, sin)
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, setFillStyle, fillRect, moveTo, lineTo, withContext, setStrokeStyle, beginPath, closePath, stroke, CanvasElement, getCanvasWidth, getCanvasHeight, clearRect)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, Ref)


-- Define 3D and 2D Point objects
newtype Point3D = Point3D
  { x :: Number
  , y :: Number
  , z :: Number
  }

newtype Point2D = Point2D
  { x :: Number
  , y :: Number
  }

newtype Angle3D = Angle3D
  { qx :: Number
  , qy :: Number
  , qz :: Number
  }

-- Define cube object
newtype Cube = Cube
  { x :: Number
  , y :: Number
  , z :: Number
  , size :: Number
  , color :: String
  }


--| Function to project 3D point on 2D coordinate plane

project :: Point3D -> Angle3D -> Point2D
project (Point3D { x, y, z }) (Angle3D { qx, qy, qz }) =
  let xRot = x * (cos qz) + y * (sin qz)
      yRot = y * (cos qz) - x * (sin qz)
      yRotQx = yRot * (cos qx) + z * (sin qx)
      zRotQx = z * (cos qx) - yRot * (sin qx)
      xRotQxQy = xRot * (cos qy) + zRotQx * (sin qy)
  in
    Point2D { x: xRotQxQy, y: yRotQx }


withStroke :: forall e.
  Context2D ->
  String ->
  (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->
  Eff (canvas :: CANVAS | e) Context2D
withStroke ctx color draw = withContext ctx do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  ctx <- closePath ctx
  stroke ctx


drawLine :: forall e. Context2D -> Point2D -> Point2D -> Eff (canvas :: CANVAS | e) Context2D
drawLine ctx (Point2D from) (Point2D to) = do
  ctx <- moveTo ctx from.x from.y
  lineTo ctx to.x to.y


drawCube :: forall e. Context2D -> Cube -> Angle3D -> Eff (canvas :: CANVAS | e) Context2D
drawCube ctx (Cube { color, x, y, z, size }) (Angle3D { qx, qy, qz })= do
  let half = size / 2.0
  let v1 = project (Point3D { x: x - half, y: y - half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v2 = project (Point3D { x: x - half, y: y + half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v3 = project (Point3D { x: x - half, y: y - half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v4 = project (Point3D { x: x - half, y: y + half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v5 = project (Point3D { x: x + half, y: y - half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v6 = project (Point3D { x: x + half, y: y + half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v7 = project (Point3D { x: x + half, y: y - half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v8 = project (Point3D { x: x + half, y: y + half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  withStroke ctx color \ctx -> do
    ctx <- drawLine ctx v1 v5
    ctx <- drawLine ctx v5 v6
    ctx <- drawLine ctx v6 v2
    ctx <- drawLine ctx v2 v1

    ctx <- drawLine ctx v3 v7
    ctx <- drawLine ctx v7 v8
    ctx <- drawLine ctx v8 v4
    ctx <- drawLine ctx v4 v3

    ctx <- drawLine ctx v1 v3
    ctx <- drawLine ctx v5 v7
    ctx <- drawLine ctx v6 v8
    drawLine ctx v2 v4


clearCanvas :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
clearCanvas canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  ctx <- getContext2D canvas
  void $ clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }


loopAnimation :: forall e state.
  Window ->
  Ref state ->
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
loopAnimation window ref state step =
  void $ requestAnimationFrame
    do loopAnimation window ref state step
       state <- readRef ref
       state <- step state
       writeRef ref state
    window


withAnimation :: forall e state.
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
withAnimation state step = do
  window <- window
  ref <- newRef state
  loopAnimation window ref state step


withAnimateContext :: forall e state.
  String ->
  state ->
  (Context2D -> state -> Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) state) ->
  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
withAnimateContext name state draw = do
  canvas <- getCanvasElementById name
  case canvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      withAnimation state \state -> do
        clearCanvas canvas
        draw ctx state
    Nothing -> pure unit


drawBackground :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawBackground ctx = do
  ctx <- setFillStyle "rgb(255,255,255)" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 1000.0, h: 700.0 }


main :: forall e. Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
main =
  let
    state = { x: 300.0
            , y: 600.0
            , qx: pi / 4.0
            , qy: pi / 3.0
            , qz: pi / 4.0
            , acc: 0.998
            , drag: false }
    canvas = getCanvasElementById "thecanvas"
  in
    withAnimateContext "thecanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: 0.0, size: 200.0, color: "rgb(0,1,0)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y, qx = state.qx + 0.005, qy = state.qy + 0.005, qz = state.qz + 0.005 }
