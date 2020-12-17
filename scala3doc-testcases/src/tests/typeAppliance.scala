package tests
package typeAppliance

trait AClass[A, B]:
  def fun[C]: AClass[A, C]

trait BClass[A, B] extends AClass[A, B]:
  override def fun[C]: BClass[A, C]

abstract class CClass[B] extends BClass[Int, B]
