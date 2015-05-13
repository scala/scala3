object Test extends dotty.runtime.LegacyApp {
  val x = 2
  println(Macros.pos(x + 2))
}