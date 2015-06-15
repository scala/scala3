object Test extends dotty.runtime.LegacyApp {
  import Macros._
  println(foo(42))
}