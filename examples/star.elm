import Turtle exposing (Step)
import Turtle.Advanced exposing (back, invisibly)
import Window
import Color exposing (..)
import Time exposing (Time)
import Debug

d : Float
d = 300

back x = Turtle.forward -x

ray : Float -> List Step
ray frac =
    let opp = frac*d
        adj = (1-frac)*d
        angle = atan2 opp adj |> (\rads -> rads*180/pi)
        travel = opp*opp + adj*adj |> sqrt
    in [ Turtle.left angle
       , Turtle.forward travel
       , invisibly (back travel)
       , Turtle.right angle
       ]

quadrant : List Step
quadrant =
    let rez = 24
        spots = List.map (\x -> x/rez) (List.range 1 rez)
        rays = List.map (\x -> Turtle.make (ray x)) spots
    in (Turtle.left 90) :: (Turtle.forward <| d * (rez-1)/rez) :: (Turtle.right 180)
        :: List.intersperse (invisibly <| Turtle.forward (d/rez)) rays

turtle = List.repeat 4 (Turtle.make quadrant)

main = Turtle.animate turtle
-- main = Signal.map (Turtle.draw turtle) Window.dimensions
