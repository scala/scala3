package tests
package objectSignatures

class A[T]
{
  val a: String = "asd"
  def method3() = "asd"
}

object A

trait C

object Base

object A2 extends A[String] with C

object <

object >

// We are not going to add final below
// final object B
