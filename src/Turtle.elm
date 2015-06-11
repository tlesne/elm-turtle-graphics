module Turtle where

{-| docs -}

import Turtle.Core as Core
import Graphics.Element exposing (Element)

type alias Step = Core.Step

left : Float -> Step
left = Core.Left

forward : Float -> Step
forward = Core.Forward

make : List Step -> Step
make = Core.Make

scale : Float -> Step
scale = Core.Scale

animate : List Step -> Signal Element
animate = Core.animate

draw : List Step -> (Int,Int) -> Element
draw = Core.draw
