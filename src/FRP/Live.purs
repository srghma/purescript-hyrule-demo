module FRP.Live
  ( createCanvas
  )
  where

import Effect
import Prelude

import Color.Scheme.MaterialDesign (blueGrey)
import Data.DateTime (Second, second)
import Data.DateTime.Instant (Instant)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Set (isEmpty)
import Data.Time.Duration (convertDuration)
import FRP.Event.Time (withTime)
import FRP.Poll (Poll, animate)
import Graphics.Canvas (CanvasElement, clearRect, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlined, render)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime as Data.DateTime

foreign import createCanvas :: Effect CanvasElement

live :: Poll Drawing -> Effect (Effect Unit)
live scene = do
  canvas <- createCanvas
  context2D <- getContext2D canvas
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  animate scene \frame -> do
    _ <- clearRect context2D { x: 0.0, y: 0.0, width, height }
    render context2D frame

-- Bits and pieces

withSeconds :: forall a. ({ value :: a, time :: Second } -> Effect Unit) -> a -> Effect Unit
withSeconds f = withTime (\({ value, time } :: { value :: a, time :: Instant }) -> f ({ value, time: second $ Data.DateTime.time $ toDateTime time }))

-- | seconds :: Poll Number
-- | seconds = ((_ / 1000.0) <<< toNumber <$> millisSinceEpoch)

-- | mouse :: Poll { x :: Number, y :: Number }
-- | mouse = position <#> maybe { x: 0.0, y: 0.0 } (\{ x, y } -> { x: toNumber x, y: toNumber y })

-- | click :: Poll Boolean
-- | click = not <<< isEmpty <$> buttons

dot :: Number -> Number -> Number -> Drawing
dot x y r = filled (fillColor blueGrey) (circle x y r) <> outlined (lineWidth (r / 4.0)) (circle x y (r * 1.2))
