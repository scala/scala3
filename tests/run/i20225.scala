sealed abstract class Parent
class A extends Parent
class B extends Parent

inline def matchAs[T <: Parent](p: Parent): Unit = p match
  case _: T => ()
  case _    => println("unreachable case reached")

object Test:
  def main(args: Array[String]): Unit =
    matchAs[A](new B)

