package extMethods

trait That1[A]
class T[A, Self <: That1[A]](val x: Int) extends AnyVal {
  self: Self =>
  final def loop(x: Self, cnt: Int): Int = loop(x, cnt + 1)
}
