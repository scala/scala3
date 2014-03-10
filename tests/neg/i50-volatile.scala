object Test {
  class Base {
    class Inner
  }
  type A <: Base {
    type X = String
  }
  type B <: {
    type X = Int
  }
  lazy val o: A & B = ???

  class Client extends o.Inner

  def xToString(x: o.X): String = x

  def intToString(i: Int): String = xToString(i)
}
object Test2 {

  import Test.o._

  def xToString(x: X): String = x

}
