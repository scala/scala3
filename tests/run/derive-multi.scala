class A
object A {
  def derived: A = {
    println("derived: A")
    new A
  }
}

class B[X, Y]
object B {
  def derived[X, Y]: B[X, Y] = {
    println("derived: B")
    new B[X, Y]
  }
}

case class One() derives A, B
case class Two() derives A, B

delegate for B[One, Two] {
  println("derived: B[One, Two]")
}

enum Lst[T] derives A, B {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

case class Triple[S, T, U] derives A, B

object Test1 {
  import Lst._
  import delegate Lst._
  implicitly[A]
}

object Test extends App {
  Test1
  implicitly[B[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[B[Triple[One, One, One],
               Triple[Two, Two, Two]]]
}
