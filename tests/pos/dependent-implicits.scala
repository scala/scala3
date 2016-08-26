object Test {
  trait T { type X; val x: X }
  implicit def f(x: T): x.X = x.x
  val t = new T { type X = String; val x = "" }
  val x: String = t
  val uy: String = f(t)
}
