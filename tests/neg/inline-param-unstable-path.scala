inline val a = 3
inline def f(inline x: Int, y: Int, z: => Int): Unit =
  val x2: x.type = x // error: (x : Int) is not a valid singleton type, since it is not an immutable path
  val y2: y.type = y
  val z2: z.type = z // error: (z : Int) is not a valid singleton type, since it is not an immutable path
  val a2: a.type = a
