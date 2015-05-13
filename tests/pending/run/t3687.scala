object t extends Enumeration { val a, b = Value }

object Test extends dotty.runtime.LegacyApp {
  println(t.values)
  println(t.values)
}
