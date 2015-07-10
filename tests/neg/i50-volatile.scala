object Test {
  class Base {
    class Inner
  }
  type A <: Base {
    type X = String                                     // error
  }
  type B <: {
    type X = Int                                        // error
  }
  lazy val o: A & B = ???

  class Client extends o.Inner                          // error // error

  def xToString(x: o.X): String = x                     // error

  def intToString(i: Int): String = xToString(i)
}
object Test2 {

  import Test.o._                                       // error

  def xToString(x: X): String = x

}
