object Bar {
  trait A
  case class A0(foo: Int) extends A
}

object Foo {
  import Bar.A

  def buzz(a: A) = {
    a match {
      case A0(_) =>     // error
    }
  }
}
