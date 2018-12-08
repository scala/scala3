class Test {
  class Base {
    class Inner
  }
  type A <: Base {
    type X = String                                     // old-error
  }
  type B <: {
    type X = Int                                        // old-error
  }
  lazy val o: A & B = ???

  class Client extends o.Inner                          // error

  def xToString(x: o.X): String = x                     // error

  def intToString(i: Int): String = xToString(i)
}

object Test2 {
  trait A {
    type X = String
  }
  trait B {
    type X = Int
  }
  lazy val o: A & B = ???

  def xToString(x: o.X): String = x       // error

  def intToString(i: Int): String = xToString(i)
}
