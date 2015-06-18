object Test extends dotty.runtime.LegacyApp {
  println(Macros.foo1.x)
  println(Macros.foo2.x)
  println(Macros.foo3.x)
}