module Turtle.Core (draw, animate, Movement, Step(..), length, depth) where

{-| A way to draw Turtle Graphics.

# Creating the Program
@docs Movement, Step

# Running the Program
@docs draw, animate

# Inspecting the Program
@docs length, depth

-}

import Graphics.Element as Element exposing (Element)
import Graphics.Collage as C exposing (defaultLine)
import Color exposing (Color)
import Window
import Random
import Time

import List.Nonempty as NE exposing (Nonempty, (:::))

-- This alias is internal to Core.
type alias Movement = List Step

{-| A Step is an action that the turtle can take.
    * Forward moves the turtle a given amount in the direction it is facing.
    * Back moves the turtle backwards by the given amount.
    * Left and Right rotate the turtle by the given angle in degrees.
    * Make tells the turtle to perform an action.
    * Branch tells the turle to perform two actions starting from the same point.
    * Scale sets the current scale factor. Moving forward and back are multiplied by the scale factor. Scale factors compound together: `Scale 2, Scale 2` is the same as `Scale 4`.
    * Stay tells the turtle to do nothing.
    * Pen sets the color of the pen's ink.
    * PenUp takes the pen off the paper so the turtle stops drawing.
    * PenDown puts the pen back on the paper.
    * Randomly provides a function with a random seed for use with the [Random](http://package.elm-lang.org/packages/elm-lang/core/latest/Random) library. The function must return the new seed.
    * Teleport moves the turtle to a new location, drawing as is goes if the pen is down.
    * RotateTo sets the turtle's rotation.

The turtle starts at (0,0) facing up (90 degrees), with the pen down using black ink, and a scale factor of 1.

-}
type Step = Forward Float | Back Float | Right Float | Left Float |
            Make Movement | Branch Movement Movement |
            Scale Float |
            Stay |
            Pen Color | PenUp | PenDown |
            Randomly (Random.Seed -> (Step, Random.Seed)) |
            Teleport (Float, Float) | RotateTo Float


type alias Coord = (Float, Float)
type alias Figure = {color : Color, path : Nonempty Coord}
type alias State = {theta : Float, scale : Float, penDown : Bool, seed : Random.Seed, figures : Nonempty Figure}

-- initial state of the evaluator
state0 : State
state0 = State (degrees 90) 1 True (Random.initialSeed 628318530718)
            <| NE.fromElement (Figure Color.black (NE.fromElement (0,0)))

-- make a new figure, based on the old one
newFigure : State -> State
newFigure state =
    let oldFigure = NE.head state.figures
        newFigure = {oldFigure| path <- NE.dropTail oldFigure.path}
    in {state| figures <- newFigure ::: state.figures}

-- Takes an action that requires a new figure. If the current figure *is* new, overwrite it; otherwise make a new one.
changeFigure : State -> (Figure -> Figure) -> State
changeFigure state f =
    if NE.isSingleton (NE.head state.figures).path
    then {state|figures <- NE.replaceHead (f <| NE.head state.figures) state.figures}
    else changeFigure (newFigure state) f

-- move to a new location, creating a new figure if the pen is up
moveTo : State -> Coord -> State
moveTo state pos =
    if not state.penDown
    then changeFigure state (\fig -> {fig| path <- NE.fromElement pos})
    else let oldFigure = NE.head state.figures
             newFigure = {oldFigure| path <- pos ::: oldFigure.path}
         in {state| figures <- NE.replaceHead newFigure state.figures}

-- evaluate a step given a state to produce a new state
eval : Step -> State -> State
eval step state = case step of
    Forward d -> let (x0, y0) = NE.head (NE.head state.figures).path
                     dx = state.scale * d * cos state.theta
                     dy = state.scale * d * sin state.theta
                 in moveTo state (x0+dx, y0+dy)
    Back d -> eval (Forward -d) state
    Right ang -> {state| theta <- state.theta - (degrees ang)}
    Left ang -> {state| theta <- state.theta + (degrees ang)}
    Scale x -> {state| scale <- state.scale * x}
    Stay -> state
    Pen color -> changeFigure state (\fig -> {fig|color <- color})
    PenUp -> {state| penDown <- False}
    PenDown -> {state| penDown <- True}
    Randomly f -> let (step', seed') = f state.seed
                  in eval step' {state|seed <- seed'}
    Teleport newPos -> moveTo state newPos
    RotateTo newTheta -> {state| theta <- (degrees newTheta)}
    Make ms -> evalFold ms state
    Branch m1 m2 ->
        let oldFigure = NE.head state.figures
            blankFigure = {oldFigure| path <- NE.dropTail oldFigure.path}
            blankState = {state| figures <- NE.fromElement blankFigure}
            results1 = evalFold m1 blankState
            results2 = evalFold m2 blankState
            newFigures = NE.Nonempty oldFigure
                <| NE.toList results1.figures ++ NE.toList results2.figures ++ NE.tail state.figures
        in {state| figures <- newFigures}

-- evaluate many steps, saving the end result
evalFold : Movement -> State -> State
evalFold = flip (List.foldl eval)

-- evaluate many steps, saving each intermediate end result
evalScan : State -> Movement -> List State
evalScan state m =
    case m of
        [] -> [state]
        step::steps -> case step of
            Make m' -> let states = evalScan state m'
                           state' = states |> List.reverse |> List.head
                                      |> Maybe.withDefault state
                       in states ++ evalScan state' steps
            _ -> let state' = eval step state
                 in state' :: evalScan state' steps

-- render the eval'd state as a collage with the given dimensions
render : (Int, Int) -> Nonempty Figure -> Element
render (w,h) figures =
    C.collage w h <|
    List.map (uncurry C.traced) <|
    List.map (\fig -> ({defaultLine| color <- fig.color}, NE.toList fig.path))
             (NE.toList figures)

{-| Run the turtle and immediately show the result in a collage of the given size (think `Window.dimensions`). Useful for rapidly iterating code, and for use with `Signal.map` and dynamic controls.
-}
draw : Movement -> (Int, Int) -> Element
draw m dims =
    render dims (evalFold m state0).figures

{-| Animate the turtle drawing by showing the progressive steps.
-}
animate : Movement -> Signal Element
animate m =
    case NE.fromList <| evalScan state0 m of
        Nothing -> Signal.constant Element.empty
        Just frames ->
            let current : Signal (Nonempty State)
                current = Signal.foldp (always NE.pop) frames (Time.fps 60)
                renderHelper dims ne_state = render dims (NE.head ne_state).figures
        in Signal.map2 renderHelper Window.dimensions current

{-| Determine the number of steps in a Movement, accounting for recursion.
-}
length : Movement -> Int
length =
    let length' step = case step of
        Make m' -> length m'
        Branch m1 m2 -> length m1 + length m2
        _ -> 1
    in List.foldl (\step sum -> length' step + sum) 0

{-| Determine the recursive depth of a Movement.
-}
depth : Movement -> Int
depth =
    let depth' step = case step of
        Make m' -> 1 + depth m'
        Branch m1 m2 -> length m1 `max` length m2 + 1
        _ -> 1
    in List.foldl (\step sum -> depth' step `max` sum) 0

