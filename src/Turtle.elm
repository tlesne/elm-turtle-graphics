module Turtle where

{-| docs -}

import Turtle.Core as Core
import Graphics.Element exposing (Element)
import Color exposing (Color)

type alias Step = Core.Step

left : Float -> Step
left = Core.Left

forward : Float -> Step
forward = Core.Forward

make : List Step -> Step
make = Core.Make

scale : Float -> Step
scale = Core.Scale

penUp : Step
penUp = Core.PenUp

penDown : Step
penDown = Core.PenDown

penColor : Color -> Step
penColor = Core.Pen

{-| Animate the turtle drawing by showing the progressive steps.
-}
animate : List Step -> Signal Element
animate = Core.animate

{-| Run the turtle and immediately show the result in a collage of the given size (think `Window.dimensions`). Useful
for rapidly prototyping code, and for use with `Signal.map` and dynamic controls.
-}
draw : List Step -> (Int,Int) -> Element
draw = Core.draw
