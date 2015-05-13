object Test extends dotty.runtime.LegacyApp {
  println(Macros.foo_with_macros_enabled)
  println(Macros.foo_with_macros_disabled)
}