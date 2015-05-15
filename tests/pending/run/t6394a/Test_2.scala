object Test extends dotty.runtime.LegacyApp {
  println(Macros.foo)
  override def toString = "TEST"
}