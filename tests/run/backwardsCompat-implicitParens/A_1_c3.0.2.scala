class Bar(using x: Int)(y: String)
object Bar:
  given Int = 1
  inline def foo =
    println(new Bar()(""))
    println(Bar()(""))
