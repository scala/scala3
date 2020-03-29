object Test {

  transparent inline def f(x: Int) = inline x match {
    case 1 => "a"
    case 2 => 22
  }

  val x: String = f(1)
  val y = f(2)
  val yc: Int = y

  transparent inline def g(x: Any) = inline x match {
    case x: String => (x, x)
    case x: Double => x
  }

  val a: (String, String) = g("")
  val b: Double = g(1.0)

}