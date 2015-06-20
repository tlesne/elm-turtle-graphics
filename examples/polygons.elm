import Turtle exposing (Step)
import Turtle.Advanced as Turtle
import Window
import Color exposing (..)
import Time exposing (Time)

polygon : Float -> Int -> Color -> Step
polygon angle n clr =
    Turtle.make [ Turtle.invisibly <| Turtle.teleport (-100*(6-toFloat n), 0)
         , Turtle.rotateTo angle
         , Turtle.penColor clr
         , Turtle.ngon n
         ]

turtle : Time -> List Step
turtle angle =
    List.map (uncurry (polygon angle)) <|
        List.map2 (,) [3..8] [red, orange, yellow, green, blue, purple]

clock : Signal Float
clock = Signal.foldp (\dt t -> dt/20 + t) 0 (Time.fps 30)

main = Signal.map (\t -> Turtle.draw (turtle t)) clock
