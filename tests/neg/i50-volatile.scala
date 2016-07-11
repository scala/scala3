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

  class Client extends o.Inner                          // old-error // old-error

  def xToString(x: o.X): String = x                     // old-error

  def intToString(i: Int): String = xToString(i)
}
object Test2 {

  import Test.o._                                       // error

  def xToString(x: X): String = x

}
