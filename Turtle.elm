module Turtle where

{-| -}

import Graphics.Element exposing (show, Element)
import Graphics.Collage as C
import Color exposing (Color)
import Window

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

turtle : Movement
turtle =
    [ Make <| ngon 8
    , Pen Color.blue
    , PenUp
    , Teleport (-100, 0)
    , PenDown
    , Make <| ngon 7
    ]

type alias Coord = (Float, Float)
type alias Figure = {color : Color, hd : Coord, tl : List Coord}
type alias ParseState = {theta : Float, scale : Float, penDown : Bool, figure : Figure, figures : List Figure}

isNewFigure : ParseState -> Bool
isNewFigure state = List.isEmpty state.figure.tl

newFigure : ParseState -> ParseState
newFigure state =
    let oldFigure = state.figure
        newFigure = {oldFigure| hd <- oldFigure.hd, tl <- []}
    in {state| figure <- newFigure, figures <- oldFigure :: state.figures}

{-| Run the tutle and immediately show the result in a collage of the size (think `Window.dimensions`). Useful for
rapidly iterating code, and for use with `Signal.map` and dynamic controls.
-}
drawTurtle : Movement -> (Int, Int) -> Element
drawTurtle m dims =
    let state0 = ParseState (degrees 90) 1 True (Figure Color.black (0,0) []) []
        parse1 : Step -> ParseState -> ParseState
        parse1 step state = case step of
            Forward d -> if not state.penDown then state else
                         let (x0, y0) = state.figure.hd
                             dx = state.scale * d * cos state.theta
                             dy = state.scale * d * sin state.theta
                             pos = (x0+dx, y0+dy)
                             oldFigure = state.figure
                             newFigure = {oldFigure| hd <- pos, tl <- oldFigure.hd :: oldFigure.tl}
                          in {state| figure <- newFigure}
            Back d -> parse1 (Forward -d) state
            Right ang -> {state| theta <- state.theta - (degrees ang)}
            Left ang -> {state| theta <- state.theta + (degrees ang)}
            Scale x -> {state| scale <- state.scale * x}
            Pen color -> if isNewFigure state
                then let oldFigure = state.figure in {state| figure <- {oldFigure|color <- color}}
                else parse1 step (newFigure state)
            PenUp -> {state| penDown <- False}
            PenDown -> {state| penDown <- True}
            Randomized f -> parse1 (f 0.5) state
            Teleport newPos -> if isNewFigure state
                then let oldFigure = state.figure in {state| figure <- {oldFigure|hd <- newPos}}
                else parse1 step (newFigure state)
            RotateTo newTheta -> {state| theta <- newTheta}
            Make ms -> parse state ms
            _ -> state
        parse : ParseState -> Movement -> ParseState
        parse = List.foldl parse1
        render (w,h) state = C.collage w h <|
            List.map (C.traced C.defaultLine) <|
            List.map (\fig -> fig.hd :: fig.tl) (state.figure :: state.figures)
        drawing : ParseState
        drawing = parse state0 m
    in render dims drawing

main = Signal.map (drawTurtle turtle) Window.dimensions
