object Test extends dotty.runtime.LegacyApp {
  println(implicitly[Foo[C1]])
  println(implicitly[Foo[C2]])
}
