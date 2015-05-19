import Turtle exposing (Movement, Step(..))
import Window
import Color exposing (..)
import Time exposing (Time)

ngon : Int -> Movement
ngon n =
    let step = Make [Forward 25, Right (360 / toFloat n)]
    in List.repeat n step

invisibly : Step -> Step
invisibly step = Make
    [ PenUp
    , step
    , PenDown
    ]

polygon : Float -> Int -> Color -> Step
polygon angle n clr =
    Make [ invisibly <| Teleport (-100*(6-toFloat n), 0)
         , RotateTo angle
         , Pen clr
         , Make <| ngon n
         ]

turtle : Time -> Movement
turtle angle =
    List.map (uncurry (polygon angle)) <|
        List.map2 (,) [3..8] [red, orange, yellow, green, blue, purple]

clock : Signal Float
clock = Signal.foldp (\dt t -> dt/20 + t) 0 (Time.fps 30)

main = Signal.map2 (\t dims -> Turtle.draw (turtle t) dims) clock Window.dimensions
