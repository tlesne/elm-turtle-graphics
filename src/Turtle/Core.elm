module Turtle.Core (DrawOptions, defaultDrawOptions, draw, AnimateOptions, defaultAnimateOptions, animate, Step(..), length, depth) where

import Graphics.Element as Element exposing (Element)
import Graphics.Collage as C exposing (defaultLine)
import Color exposing (Color)
import Window
import Random
import Time

import List.Nonempty as NE exposing (Nonempty, (:::))

-- this alias is internal to Core.
type alias Movement = List Step

-- language primitives. The nodes of the AST.
type Step = Forward Float | Back Float | Right Float | Left Float |
            Make Movement | Branch Movement Movement | Atomically Movement |
            Scale Float |
            Stay |
            Pen Color | PenUp | PenDown |
            Randomly (Random.Seed -> (Step, Random.Seed)) |
            Teleport (Float, Float) | RotateTo Float

type alias Coord = (Float, Float)
type alias Figure = {color : Color, path : Nonempty Coord}
type alias State = {theta : Float, scale : Float, penDown : Bool, seed : Random.Seed, figures : Nonempty Figure}

-- a random seed, good as any
seed : Random.Seed
seed = Random.initialSeed 628318530718

-- initial state of the evaluator, parameterized over the seed
state0 : Random.Seed -> State
state0 seed = State (degrees 90) 1 True seed
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
    Atomically ms -> evalFold ms state
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

-- TODO put this in the Nonempty library
ne_append (NE.Nonempty x xs) (NE.Nonempty y ys) = NE.Nonempty x (xs ++ y :: ys)

-- evaluate many steps, saving each intermediate end result
evalScan : State -> Movement -> Nonempty State
evalScan state m =
    case m of
        [] -> NE.fromElement state
        step::steps -> case step of
            Make m' -> let states = evalScan state m'
                           state' = states |> NE.reverse |> NE.head
                       in states `ne_append` evalScan state' steps
            _ -> let state' = eval step state
                 in state' ::: evalScan state' steps

-- render the eval'd state as a collage with the given dimensions
render : (Int, Int) -> Nonempty Figure -> Element
render (w,h) figures =
    C.collage w h <|
    List.map (uncurry C.traced) <|
    List.map (\fig -> ({defaultLine| color <- fig.color}, NE.toList fig.path))
             (NE.toList figures)

type alias DrawOptions = { seed : Random.Seed , dims : (Int, Int) }

defaultDrawOptions : DrawOptions
defaultDrawOptions = DrawOptions seed (800, 800)

-- run the turtle and immediately show the result in a collage of the given size
draw : DrawOptions -> Movement ->  Element
draw {seed, dims} m =
    render dims (evalFold m (state0 seed)).figures

type alias AnimateOptions = { seed : Random.Seed , dims : Signal (Int, Int), clock : Signal () }

defaultAnimateOptions : AnimateOptions
defaultAnimateOptions = AnimateOptions seed Window.dimensions (Signal.map (always ()) (Time.fps 5))

-- animate the turtle drawing by showing the progressive steps
animate : AnimateOptions -> Movement -> Signal Element
animate {seed, dims, clock} m =
    let noPath fig = NE.isSingleton fig.path
        figures : Nonempty (Nonempty Figure)
        figures = evalScan (state0 seed) m
            -- pruning logic to remove duplicate paths, paths of one point, and duplicate figures
            |> NE.map (.figures>>(\figs -> if noPath (NE.head figs) then NE.pop figs else figs)>>NE.dedup)
            |> NE.dedup
        current : Signal (Nonempty Figure)
        current = Signal.foldp (always NE.pop) figures clock |> Signal.map NE.head
    in Signal.map2 render dims current

-- determine the number of steps in a Movement, accounting for recursion
length : Movement -> Int
length =
    let length' step = case step of
        Make m' -> length m'
        Branch m1 m2 -> length m1 + length m2
        _ -> 1
    in List.foldl (\step sum -> length' step + sum) 0

-- determine the recursive depth of a Movement
depth : Movement -> Int
depth =
    let depth' step = case step of
        Make m' -> depth m' + 1
        Branch m1 m2 -> depth m1 `max` depth m2 + 1
        _ -> 1
    in List.foldl (\step sum -> depth' step `max` sum) 0

