trait Outer extends Any {
  trait Inner1
  trait Inner2 extends Any
  class Inner3
  class Inner4(a: Int) extends AnyVal // error
  case class Inner5(a: Int) // error
  object Inner6 // error
}
