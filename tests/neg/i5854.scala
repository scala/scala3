class B {
  val a: String = (((1: Any): b.A): Nothing): String  // error
  val b: { type A >: Any <: Nothing } = loop()
  def loop(): Nothing = loop()
}
