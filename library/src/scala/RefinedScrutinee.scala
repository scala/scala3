package scala

class RefinedScrutinee[+Scrutinee /*<: Singleton*/, +Result] private (val result: Option[Result]) extends AnyVal {
  // Scrutinee in not a singleton to provide a better error message

  /** There is no result */
  def isEmpty: Boolean = result.isEmpty

  /** Get the result */
  def get: Result = result.get
}

object RefinedScrutinee {
  // TODO when bootstrapped: erase scrutinee as it is just an evidence for that existance of a term of type Scrutinee
  def matchOf[Scrutinee <: Singleton, Result](/*erased*/ scrutinee: Scrutinee)(result: Result): RefinedScrutinee[Scrutinee, Result] = new RefinedScrutinee(Some(result))
  def noMatch[Scrutinee <: Singleton, Result]: RefinedScrutinee[Scrutinee, Result] = new RefinedScrutinee(None)
}
