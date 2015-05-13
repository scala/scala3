object Test extends dotty.runtime.LegacyApp {
  val list: List[String] = Macros.foo("hello world")
  println(list)
}