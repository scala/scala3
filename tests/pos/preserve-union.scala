class A {
  val a: Int | String = 1
  val b: AnyVal = 2

  val c = List(a, b)
  val c1: List[AnyVal | String] = c
}
