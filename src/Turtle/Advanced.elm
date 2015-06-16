module Turtle.Advanced where

{-| docs -}

import Turtle.Core as Core exposing (Step)

right : Float -> Step
right = Core.Right

back : Float -> Step
back = Core.Back

rotateTo : Float -> Step
rotateTo = Core.RotateTo

teleport : (Float, Float) -> Step
teleport = Core.Teleport

ngon : Int -> Step
ngon n =
    Core.Make <| List.repeat n <| Core.Make [Core.Forward 1, Core.Left (360 / toFloat n)]

circle : Float -> Step
circle r =
    let n = 64
        dc = pi*r/n
    in Core.Atomically <| (Core.Forward r) :: (Core.Left 90) :: List.repeat n (Core.Make [Core.Forward dc, Core.Left (360 / toFloat n)])

scaled : Float -> Step -> Step
scaled factor step =
    Core.Make [Core.Scale factor, step, Core.Scale (1/factor)]

invisibly : Step -> Step
invisibly step =
    Core.Make [Core.PenUp, step, Core.PenDown]

{-| The same as `make`, except when using `animate` the steps are all run together (atomically). Used by `circle` and
`ngon` to draw shapes immediately rather than watch the turtle trace them out.
-}
atomically : List Step -> Step
atomically = Core.Atomically

{-| Determine the number of steps in a Movement, accounting for recursion.
-}
length : List Step -> Int
length = Core.length

{-| Determine the recursive depth of a Movement.
-}
depth : List Step -> Int
depth = Core.depth
