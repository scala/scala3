object Test extends dotty.runtime.LegacyApp {
  for (i <- 0 to 1) {
    val a = Foo
  }
}

object Foo {
  println("hello")
}
