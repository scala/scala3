object Test extends dotty.runtime.LegacyApp {
  def foo(f: String => Array[String])(s: String) = f(s)
  val test = foo(Array(_)) _
  println(test("x").toList)
}
