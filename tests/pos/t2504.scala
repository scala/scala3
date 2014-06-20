object Test {
  val zs = Array("abc")
  val ys: Iterable[_] = Array("abc")
  val xs = Array("abc")
  xs sameElements Array("abc")
}
