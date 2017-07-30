module Main exposing (..)

import Turtle exposing (Step)
import Turtle.Advanced as Turtle


main =
    Turtle.animate [ Turtle.left 30, Turtle.star 6 2, Turtle.invisibly (Turtle.forward 200), Turtle.star 5 3 ]
