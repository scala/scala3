object Test extends dotty.runtime.LegacyApp {
  println("foo")
  Macros.foo(42)
}