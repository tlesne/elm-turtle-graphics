module Turtle where

{-|

Move the turtle around by making a list of steps. Then give it to the turtle with `animate`. Like this:

````elm
import Turtle exposing (..)

steps = [forward 20, left 90, forward 10]
main = animate steps
````

The turtle starts at (0,0) facing up (90 degrees), with the pen down using black ink, and a scale factor of 1.

# Making Steps
@docs left, forward, make, penUp, penDown, penColor, withRandom

# Running Steps
Once you've made a list of steps, use one of these functions to see it happen.
@docs animate, draw
-}

import Turtle.Core as Core
import Graphics.Element exposing (Element)
import Color exposing (Color)
import Random

type alias Step = Core.Step

{- Turn the turtle left by a certain number of degrees. How can you make the turtle turn right?
-}
left : Float -> Step
left = Core.Left

{- Move the turtle forward by a certain amount. How can you make the turtle move backwards?
-}
forward : Float -> Step
forward = Core.Forward

{- Do every step in the list. Really useful for giving names to small jobs. Think about `make star`, or making a
five-pointed star with `make (star 5)`.
-}
make : List Step -> Step
make = Core.Make

{-| Make moving forward bigger or smaller. The numbers multiply, so doing `scale 3` and then `scale 5` is the same as
doing `scale 15`.
-}
scale : Float -> Step
scale = Core.Scale

{-| Lift the pen from the paper so that nothing is drawn.
-}
penUp : Step
penUp = Core.PenUp

{-| Put the pen back down so that the turtle starts drawing again.
-}
penDown : Step
penDown = Core.PenDown

{-| Change the color of the drawing.
-}
penColor : Color -> Step
penColor = Core.Pen

{-| Provide a step-making function with a random number between 0 and 1.
-}
withRandom : (Float -> Step) -> Step
withRandom =
    let gen = Random.float 0 1
    in (\f -> Core.Randomly (\seed ->
        let (x, seed') = Random.generate gen seed
        in (f x, seed')))

{-| Animate the turtle drawing by showing the each step.
-}
animate : List Step -> Signal Element
animate = Core.animate

{-| Run the turtle and immediately show the result in a collage of the given size. It is
useful when you are changing your code quickly: `main = Signal.map (draw mySteps) Window.dimensions`
-}
draw : List Step -> (Int,Int) -> Element
draw = Core.draw
