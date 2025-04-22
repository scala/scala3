object HammerSpec {
  case class A(x: Int)
  case class B(x: Int)
  Hammer.makeHammer[A, B]()
}
