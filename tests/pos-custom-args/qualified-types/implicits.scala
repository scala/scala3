type Pos = { v: Int with v >= 0 }
type Neg = { v: Int with v < 0 }

trait Show[-A]:
  def apply(a: A): String

given show1: Show[Pos] with
  def apply(a: Pos): String = "I am a positive integer!"

given show2: Show[Neg] with
  def apply(a: Neg): String = "I am a negative integer!"

def show[A](a: A)(using s: Show[A]): String = s.apply(a)

def f(x: Int with x == 42, y: Int with y == -42): Unit =
  println(show(x)) // I am a positive integer!
  println(show(y)) // I am a negative integer!
