import Turtle
import Turtle.Advanced as Turtle

main = Turtle.animate
    [Turtle.scaled 50 <| Turtle.make (List.map Turtle.ngon [3..32])]
