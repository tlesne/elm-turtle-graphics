module Turtle where

{-| docs -}

import Turtle.Core as Core
import Graphics.Element exposing (Element)

type alias Step = Core.Step

forward : Float -> Step
forward = Core.Forward

animate : List Step -> Signal Element
animate = Core.animate

draw : List Step -> (Int,Int) -> Element
draw = Core.draw
