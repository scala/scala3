object Test {
  transparent inline def not(x: Boolean): Boolean = {
    !x
  }

  final val a = not(true)
  val b: false = a

  transparent inline def add(x: Int, y: Int): Int = {
    x + y
  }

  final val c = add(3, 4)
  val d: 7 = c
}
