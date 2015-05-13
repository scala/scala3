object Test extends dotty.runtime.LegacyApp {
  println(Macros.foo_with_implicits_enabled)
  println(Macros.foo_with_implicits_disabled)
}