package tests
package typeAppliance

trait AClass[A, B]:
  def funASD[C, D](f: B => C): AClass[A, C]

trait BClass[A, B] extends AClass[A, B]:
  override def funASD[X, D](f: B => X): BClass[A, X]
  val f: (=> B) => String
  = _ => "abc"


abstract class CClass[U] extends BClass[Int, U]:
  def someFun(n: Int)(b: Int): Int
  = 1

