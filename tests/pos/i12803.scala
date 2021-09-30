trait X {
  type Y
}

trait E[A]

trait Test {
  val x: X
  def wrap(x: X): E[x.Y] = ???
  def run[I](i: E[I]): Unit = ???
  run(wrap(x))
}
