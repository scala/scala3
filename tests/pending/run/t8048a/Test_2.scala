object Test extends dotty.runtime.LegacyApp {
  val x: Option[Int] = Macros.foo
  println(x)
}