object Test extends dotty.runtime.LegacyApp {
  println("foo_targs:")
  new Macros[Int]().foo_targs[String](42)
}