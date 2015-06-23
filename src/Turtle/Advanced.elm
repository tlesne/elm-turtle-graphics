module Turtle.Advanced where

{-| Advanced turtle drawing functions for those comfortable programming in Elm. This module is meant to be combined with
the simple one, so either import both `exposing (..)` or:

````elm
import Turtle exposing (Step)
import Turtle.Advanced as Turtle
````

Yes, this will work and place all the functions into the `Turtle` namespace.

# Basics
@docs back, stay

# Shapes
@docs ngon, circle, star

# Functional Modifiers
These functions wrap steps with transformations that are undone afterwards, rather than leaving state with the turtle.
They still leak state a little, so be careful. Also, they may be made to leak less in a MINOR release.
@docs scaled, rotated, invisibly

# Atomicity
@docs atomically

# Absolute Positioning
@docs teleport, rotateTo

# Working with Steps
## Drawing
@docs DrawOptions, defaultDrawOptions, drawWith

## Animating
@docs AnimateOptions, defaultAnimateOptions, animateWith

## Inspecting
@docs length, depth

-}

import Turtle.Core as Core exposing (Step)
import Graphics.Element exposing (Element)
import Random

{-| Move the turtle back a distance, without affecting the rotation. The turtle will still draw the path if the pen is
down.
-}
back : Float -> Step
back = Core.Back

{-| Tell the turtle to do nothing.
-}
stay : Step
stay = Core.Stay

{-| Provide access to a random seed, and therefore, the Random library. This is much more powerful than `withRandom`
taking a random float, but carries with it the responsibility to pass back an unused seed.
-}
randomly : (Random.Seed -> (Step, Random.Seed)) -> Step
randomly = Core.Randomly

{-| Rotate to a given angle in degrees, regardless of the old rotation.
-}
rotateTo : Float -> Step
rotateTo = Core.RotateTo

{-| Move to a given location, regardless of the old location.
-}
teleport : (Float, Float) -> Step
teleport = Core.Teleport

{-| Create a regular n-gon with sides of length 50. Use `scale` or `scaled` to increase the size. The turtle begins the
first edge immediately; you may want to position and rotate the turtle first.
-}
ngon : Int -> Step
ngon n =
    Core.Make <| List.repeat n <| Core.Make [Core.Forward 50, Core.Left (360 / toFloat n)]

{-| Create a circle of the given radius, centered at the current location.
-}
circle : Float -> Step
circle r =
    let n = 64
        dc = pi*r/n
    in Core.Atomically <| (Core.Forward r) :: (Core.Left 90) :: List.repeat n (Core.Make [Core.Forward dc, Core.Left (360 / toFloat n)])


{-| Create a star polygon. The first argument specifies the number of points. The second argument specifies the ratio of
the exterior angle to the interior angle. A pentagram is `star 5 3` and a Star of David is `star 6 2`. This can take
some experimentation to get the effect you need; sometimes a star comes out as a convex polygon.

Each side has length 50. Use `scale` or `scaled` to increase the size. The turtle begins the
first edge immediately; you may want to position and rotate the turtle first.
-}
star : Int -> Float -> Step
star n m =
    let alpha = 360 / (toFloat n * (m - 1))
        beta = m*alpha
    in Core.Atomically <| List.repeat n <| Core.Make
        [ Core.Forward 50
        , Core.Right (180-alpha)
        , Core.Forward 50
        , Core.Left (180-beta)
        ]

{-| Scale a step by a given factor without affecting later actions (assuming the step itself does not change the scale).
-}
scaled : Float -> Step -> Step
scaled factor step =
    Core.Make [Core.Scale factor, step, Core.Scale (1/factor)]

{-| Rotate a step left (counterclockwise) by a given angle in degrees, and then rotate back (assuming the step itself does not change the angle.)
-}
rotated : Float -> Step -> Step
rotated angle step =
    Core.Make [Core.Left angle, step, Core.Right angle]

{-| Run a step with the pen up, and then put it down to start drawing. Useful for moving the a new location.
-}
invisibly : Step -> Step
invisibly step =
    Core.Make [Core.PenUp, step, Core.PenDown]

{-| The same as `make`, except when using `animate` the steps are all run together (atomically). Also considered to be a
single step by `depth` and `length`. Used by the geometry helpers in this library to draw shapes immediately rather than
watch the turtle trace them out.
-}
atomically : List Step -> Step
atomically = Core.Atomically

{-| Customize the `drawWith` function by supplying the initial random seed and the dimensions of the drawing.
-}
type alias DrawOptions = { seed : Random.Seed , dims : (Int, Int) }

{-| The options used for `draw`. Uses an arbitrary seed and the arbitrary dimensions of 1000x1000.
-}
defaultDrawOptions : DrawOptions
defaultDrawOptions = Core.defaultDrawOptions

{-| Draw the steps immediately, using the options given. You will usually use this with `Signal.map` to rapidly iterate
a design, change a drawing based on user input, or create a custom animation.
-}
drawWith : DrawOptions -> List Step ->  Element
drawWith = Core.draw

{-| Customize the `animateWith` function by supplying the initial random seed, the dimensions of the drawing, and the
clock on which the animation progresses.
-}
type alias AnimateOptions = { seed : Random.Seed , dims : Signal (Int, Int), clock : Signal () }

{-| The options used for `animate`. Uses the same seed as the draw options, `Window.dimensions`, and a 5fps timer. You
can use a faster timer, or `Mouse.clicks`, among other things.
-}
defaultAnimateOptions : AnimateOptions
defaultAnimateOptions = Core.defaultAnimateOptions

{-| Animate the steps running sequentially, using the options given. Useful if `animate` isn't quite meeting your needs.
-}
animateWith : AnimateOptions -> List Step ->  Signal Element
animateWith = Core.animate

{-| Determine the number of steps in a list of Steps, accounting for recursion.
-}
length : List Step -> Int
length = Core.length

{-| Determine the recursive depth of a list of Steps.
-}
depth : List Step -> Int
depth = Core.depth
