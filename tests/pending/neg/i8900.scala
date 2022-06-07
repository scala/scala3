trait Base {
  type M
}
trait A {
  type M >: Int | String
}
trait B {
  type M <: Int & String
}
object Test {
  def foo[T](z: T, x: A & B => T): T = z
  def foo2[T](z: T, x: T): T = z

  def main(args: Array[String]): Unit = {
    val x = foo(1, x => (??? : x.M))
    val x1: String = x // error (was: ClassCastException)

    val a = foo2(1,
      if false then
        val x: A & B = ???
        ??? : x.M
      else 1
    )

    val b: String = a // error (was: ClassCastException)
  }
}

