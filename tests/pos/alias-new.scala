object O {
  class B[T]
}
object O2 {
  type B[T] = O.B[T]
}
object Test {
  val x: O2.B[String] = new O2.B()
}