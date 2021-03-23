package a

object A {

  trait Fn[-T1, +R] { def apply(v1: T1): R }

  val fn0: Fn[Int, Int] { def apply(arg: Int): Int } = ???

  val fn = fn0

}
