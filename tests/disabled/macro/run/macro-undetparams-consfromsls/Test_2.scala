object Test extends dotty.runtime.LegacyApp {
  import Macros._
  val xs = cons(1, nil)
  println(xs)
  val ys = cons("abc", xs)
  println(ys)
}