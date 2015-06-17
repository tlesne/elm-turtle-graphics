import Turtle exposing (Step)
import Turtle.Advanced as Turtle

r = 100

main = Turtle.animate (star 5)

star n =
    let dt = 360 / toFloat n
    in (Turtle.right 90 :: List.repeat n (Turtle.make [Turtle.forward r, Turtle.right (2*dt), Turtle.forward r, Turtle.left dt]))
