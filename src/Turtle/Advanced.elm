module Turtle.Advanced where

{-| docs -}

import Turtle.Core as Core exposing (Step)

rotateTo : Float -> Step
rotateTo = Core.RotateTo

teleport : (Float, Float) -> Step
teleport = Core.Teleport

ngon : Int -> Step
ngon n =
    Core.Make <| List.repeat n <| Core.Make [Core.Forward 1, Core.Left (360 / toFloat n)]

scaled : Float -> Step -> Step
scaled factor step =
    Core.Make [scale factor, step, scale (1/factor)]

invisibly : Step -> Step
invisibly step =
    Core.Make [Core.PenUp, step, Core.PenDown]

circle : Float -> Step
circle r =
    let n = 64
    in Core.Stay -- not implemented yet
