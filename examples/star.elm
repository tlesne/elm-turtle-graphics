import Turtle exposing (Movement, Step(..))
import Window
import Color exposing (..)
import Time exposing (Time)
import Debug

d : Float
d = 300

ray : Float -> Movement
ray frac =
    let opp = frac*d
        adj = (1-frac)*d
        angle = atan2 opp adj |> (\rads -> rads*180/pi)
        travel = opp*opp + adj*adj |> sqrt
    in [ Left angle
       , Forward travel
       , Back travel
       , Right angle
       ]

quadrant : Movement
quadrant =
    let rez = 24
        spots = List.map (\x -> x/rez) [1..rez]
        rays = List.map (\x -> Make (ray x)) spots
    in (Left 90) :: (Forward <| d * (rez-1)/rez) :: (Right 180)
        :: List.intersperse (Forward (d/rez)) rays

turtle = List.repeat 4 (Make quadrant)

main = Signal.map (Turtle.draw turtle) Window.dimensions
