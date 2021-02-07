package tests
package fieldsSignatures

case class A(cA: String, var cB: Int)
{
  val A: Int
    = 1
  val B: Int
   = 2
  var other: Int
   = 4
}

trait C
{
  val d: Int
}

abstract class D extends C
{
  override val d: Int
   = 1
}

trait C2
{
  def d: Int
}


abstract class D2 extends C
{
  override val d: Int
  = 1
}

object Documentation
{
  val valInsideDocObject: Nothing
    = ???
}
