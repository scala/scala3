object Test {

  case class C(x: Int)

  val c: Any = C(1)

  val C(x) = c

}
