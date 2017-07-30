module Turtle exposing (..)

{-| A tiny language for teaching a virtual turtle to draw. The turtle carries a pen and draws lines as it moves.

Move the turtle around by making a list of steps. Then give it to the turtle with `animate`. Like this:

    import Turtle exposing (..)

    steps =
        [ forward 20, left 90, forward 10 ]

    main =
        animate steps

The turtle starts in the middle of the page facing up (90 degrees), with the pen down using black ink.


# Moving

@docs left, right, forward


# Drawing

@docs penUp, penDown, penColor


# Special

@docs make, withRandom, scale


# Running Steps

Once you've made a list of steps, use one of these functions to see it happen.
@docs animate, draw


# What is a Step?

@docs Step

-}

import Turtle.Core as Core
import Element exposing (Element)
import Color exposing (Color)
import Random


{-| A Step is an action that a turtle can do.
-}
type alias Step =
    Core.Step


{-| Turn the turtle left by a certain number of degrees.
-}
left : Float -> Step
left =
    Core.Left


{-| Turn the turtle right by a certain number of degrees.
-}
right : Float -> Step
right =
    Core.Right


{-| Move the turtle forward by a certain amount. How can you make the turtle move backwards?
-}
forward : Float -> Step
forward =
    Core.Forward


{-| Do every step in the list. Really useful for giving names to small jobs. Think about `make star`, or making a
five-pointed star with `make (star 5)`.
-}
make : List Step -> Step
make =
    Core.Make


{-| Make moving forward bigger or smaller. The numbers multiply, so doing `scale 3` and then `scale 5` is the same as
doing `scale 15`.
-}
scale : Float -> Step
scale =
    Core.Scale


{-| Lift the pen from the paper so that nothing is drawn.
-}
penUp : Step
penUp =
    Core.PenUp


{-| Put the pen back down so that the turtle starts drawing again.
-}
penDown : Step
penDown =
    Core.PenDown


{-| Change the color of the drawing.
-}
penColor : Color -> Step
penColor =
    Core.Pen


{-| Give a step-making function a random number between 0 and 1.
-}
withRandom : (Float -> Step) -> Step
withRandom =
    let
        gen =
            Random.float 0 1
    in
        (\f ->
            Core.Randomly
                (\seed ->
                    let
                        ( x, seed_ ) =
                            Random.generate gen seed
                    in
                        ( f x, seed_ )
                )
        )


{-| Animate the turtle drawing, showing the each step as it moves.
-}
animate : List Step -> Signal Element
animate =
    Core.animate Core.defaultAnimateOptions


{-| Run the turtle and immediately show the result. It is useful when you are
changing your code quickly and don't want to wait for the turtle to move.
-}
draw : List Step -> Element
draw =
    Core.draw Core.defaultDrawOptions
