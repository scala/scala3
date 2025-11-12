//> using options -source:future

trait L[+A]{val a:A}
trait R[+B]{val b: B}

class LR(val a: Int, val b: String) extends L[Int] with R[String]

type E[+A] = L[A] | R[A]

val x: E[Int] & E[String] = LR(4, "hi")
val y: E[Int&String] = x // error

val z = y match
  case l : L[Int&String] => l.a
  case r : R[Int&String] => r.b

val _ = z:String // was: java.lang.ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String
