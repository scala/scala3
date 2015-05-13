object Test extends dotty.runtime.LegacyApp {
  println(Macros.fooUU(2))
  println(Macros.fooTU(2))
  println(Macros.fooUT(2))
  println(Macros.fooTT(2))
}