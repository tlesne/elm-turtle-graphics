module Turtle where

{-| -}

import Graphics.Element exposing (show, Element)
import Graphics.Collage as C exposing (defaultLine)
import Color exposing (Color)
import Window
import Debug

import List.Nonempty as NE exposing (Nonempty, (:::))

type alias Movement = List Step
type Step = Forward Float | Back Float | Right Float | Left Float |
            Make Movement |
            Scale Float |
            Pen Color | PenUp | PenDown |
            Randomized (Float -> Step) |
            Teleport (Float, Float) | RotateTo Float

corner : Movement
corner =
  [ Forward 50
  , Right 90
  ]

square : Movement
square =
  List.repeat 4 (Make corner)

ngon : Int -> Movement
ngon n =
    let step = Make [Forward 25, Right (360 / toFloat n)]
    in List.repeat n step

invisibly : Step -> Step
invisibly x = Make
    [ PenUp
    , x
    , PenDown
    ]

turtle : Movement
turtle =
    [ Make <| ngon 8
    , Pen Color.blue
    , invisibly <| Teleport (-100, 0)
    , Make <| ngon 7
    , invisibly <| Teleport (-300, 0)
    , Pen Color.red
    , Make <| ngon 9
    ]

type alias Coord = (Float, Float)
type alias Figure = {color : Color, path : Nonempty Coord}
type alias ParseState = {theta : Float, scale : Float, penDown : Bool, figures : Nonempty Figure}

isNewFigure : ParseState -> Bool
isNewFigure state = NE.isSingleton (NE.head state.figures).path

newFigure : ParseState -> ParseState
newFigure state =
    let oldFigure = NE.head state.figures
        newFigure = {oldFigure| path <- NE.dropTail oldFigure.path}
    in {state| figures <- newFigure ::: state.figures}

changeFigure : ParseState -> (Figure -> Figure) -> ParseState
changeFigure state f =
    if NE.isSingleton (NE.head state.figures).path
    then {state|figures <- NE.replaceHead (f <| NE.head state.figures) state.figures}
    else changeFigure (newFigure state) f

{-| Run the turtle and immediately show the result in a collage of the given size (think `Window.dimensions`). Useful
for rapidly iterating code, and for use with `Signal.map` and dynamic controls.
-}
drawTurtle : Movement -> (Int, Int) -> Element
drawTurtle m dims =
    let state0 : ParseState
        state0 = ParseState (degrees 90) 1 True <| NE.fromElement (Figure Color.black (NE.fromElement (0,0)))
        moveTo : ParseState -> Coord -> ParseState
        moveTo state pos =
            if not state.penDown
            then changeFigure state (\fig -> {fig| path <- NE.fromElement pos})
            else let oldFigure = NE.head state.figures
                     newFigure = {oldFigure| path <- pos ::: oldFigure.path}
                 in {state| figures <- NE.replaceHead newFigure state.figures}
        parse1 : Step -> ParseState -> ParseState
        parse1 step state = case step of
            Forward d -> let (x0, y0) = NE.head (NE.head state.figures).path
                             dx = state.scale * d * cos state.theta
                             dy = state.scale * d * sin state.theta
                         in moveTo state (x0+dx, y0+dy)
            Back d -> parse1 (Forward -d) state
            Right ang -> {state| theta <- state.theta - (degrees ang)}
            Left ang -> {state| theta <- state.theta + (degrees ang)}
            Scale x -> {state| scale <- state.scale * x}
            Pen color -> changeFigure state (\fig -> {fig|color <- color})
            PenUp -> {state| penDown <- False}
            PenDown -> {state| penDown <- True}
            Randomized f -> parse1 (f 0.5) state
            Teleport newPos -> moveTo state newPos
            --let fig = NE.head state.figures in {state| figures <- NE.replaceHead {fig|path <- newPos ::: fig.path} state.figures}
            RotateTo newTheta -> {state| theta <- newTheta}
            Make ms -> parse state ms
            _ -> state
        parse : ParseState -> Movement -> ParseState
        parse = List.foldl parse1
        render (w,h) state = C.collage w h <|
            List.map (uncurry C.traced) <|
            List.map (\fig -> ({defaultLine| color <- fig.color}, NE.toList fig.path)) (NE.toList state.figures)
        drawing : ParseState
        drawing = parse state0 m
        _ = Debug.log "number of figures" (NE.length drawing.figures)
        _ = Debug.log "drawing" drawing
    in render dims drawing

main = Signal.map (drawTurtle turtle) Window.dimensions
