module Main where

import Data.Newtype (unwrap)
import Prelude

import Color (lighten)
import Color.Scheme.MaterialDesign (blueGrey)
import Data.Array (sortBy, (..))
import Data.DateTime.Instant (unInstant)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Set (Set, isEmpty)
import Data.Set as Set
import Effect (Effect)
import Effect.Now (now)
import FRP.Event (Event, subscribe)
import FRP.Event.AnimationFrame (animationFrame')
import FRP.Event.Mouse (Buttons, getMouse, readButtons, readPosition)
import FRP.Live (createCanvas)
import FRP.Poll.Unoptimized (Poll, fixB, integral', sampleBy, sample_, sham)
import Graphics.Canvas (clearRect, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate)

type Circle = { x :: Number, y :: Number, size :: Number }

-- { position :: Ref.Ref (Maybe { x :: Int, y :: Int })
-- , buttons :: Ref.Ref (Set.Set Int)
-- }

scene
  :: { w :: Number, h :: Number }
  -> Event { position :: Maybe { x :: Int, y :: Int }, buttons :: Set.Set Int, seconds :: Number }
  -> Poll Drawing
scene { w, h } inputEvent = pure background <> map renderCircles circles
  where
  background :: Drawing
  background = filled (fillColor blueGrey) (rectangle 0.0 0.0 w h)

  scaleFactor :: Number
  scaleFactor = max w h / 16.0

  renderCircle :: Circle -> Drawing
  renderCircle { x, y, size } =
    scale scaleFactor scaleFactor <<< translate x y <<< scale size size $
      outlined
        (outlineColor (lighten (0.2 + size * 0.2) blueGrey) <> lineWidth ((1.0 + size * 2.0) / scaleFactor))
        (circle 0.0 0.0 0.5)

  renderCircles :: Array Circle -> Drawing
  renderCircles = foldMap renderCircle

  -- `swell` is an interactive function of time defined by a differential equation:
  --
  -- d^2s/dt^2
  --   | mouse down = ⍺ - βs
  --   | mouse up   = ɣ - δs - ε ds/dt
  --
  -- So the function exhibits either decay or growth depending on if
  -- the mouse is pressed or not.
  --
  -- We can solve the differential equation by integration using `solve2'`.
  swell :: Poll Number
  swell =
      fixB 2.0 \b ->
        integral' 2.0 secondsPoll
          let db = fixB 10.0 \db_ ->
                     integral' 10.0 secondsPoll (f <$> buttonsPoll <*> b <*> db_)
          in db
          -- in switcher db (buttonPressedEvent $> db)
    where
      secondsPoll :: Poll Number
      secondsPoll = sham (_.seconds <$> inputEvent)

      buttonsPoll :: Poll Buttons
      buttonsPoll = sham (_.buttons <$> inputEvent)

      -- buttonPressedEvent :: Event Unit
      -- buttonPressedEvent = ?a (isEmpty) $ _.buttons <$> inputEvent

      f :: Set Int -> Number -> Number -> Number
      f bs s ds | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
                | otherwise = 2.0 * (4.0 - s)

  circles :: Poll (Array Circle)
  circles = sampleBy (\sw input -> toCircles sw input.position) swell (sham inputEvent)
    where
    toCircles :: Number -> Maybe { x :: Int, y :: Int } -> Array { size :: Number, x :: Number, y :: Number }
    toCircles sw xy =
        sortBy (comparing (\{ x, y } -> -(dist x y xy))) do
          i <- 0 .. 16
          j <- 0 .. 16
          let x = toNumber i
              y = toNumber j
              d = dist x y xy
          pure { x
               , y
               , size: 0.1 + (1.0 + sw) / (d + 1.5)
               }
      where
        dist :: Number -> Number -> Maybe { x :: Int, y :: Int } -> Number
        dist x y = maybe top \{ x: mx, y: my } ->
          let dx = x - toNumber mx / scaleFactor
              dy = y - toNumber my / scaleFactor
          in dx * dx + dy * dy

main :: Effect Unit
main = do
  -- canvas <- getCanvasElementById "canvas" >>=
  --   maybe (throwException $ error "Canvas not found") pure
  canvas <- createCanvas
  context2D <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth canvas w
  _ <- setCanvasHeight canvas h
  mouse <- getMouse

  ({ event } :: { event :: Event _, unsubscribe :: Effect Unit }) <- animationFrame' \callback -> do
    position <- readPosition mouse
    buttons <- readButtons mouse
    seconds <- (\x -> unwrap (unInstant x) / 1000.0) <$> now
    callback { buttons, position, seconds }

  _ <- subscribe (sample_ (scene { w, h } event) event) \input -> do
    clearRect context2D { x: 0.0, y: 0.0, width: w, height: w }
    render context2D input

  pure unit
