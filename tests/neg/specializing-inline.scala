object Test {

  inline def h(x: Boolean) = if (x) 1 else ""
  val z = h(true)
  val zc: Int = z // error

  transparent inline def g: Any = 1
  val y = g
  val yc: Int = y // OK

  inline def f: Any = 1
  val x = f
  val xc: Int = x // error
}