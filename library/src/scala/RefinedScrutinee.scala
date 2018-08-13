package scala

final class RefinedScrutinee[+Scrutinee /*<: Singleton*/, +Result] private (val result: Any) extends AnyVal {
  // Scrutinee in not a singleton to provide a better error message

  /** There is no result */
  def isEmpty: Boolean = result == RefinedScrutinee.NoResult

  /** Get the result */
  def get: Result = result.asInstanceOf[Result]
}

object RefinedScrutinee {
  private[RefinedScrutinee] object NoResult

  def matchOf[Scrutinee <: Singleton, Result](scrutinee: Scrutinee)(result: Result): RefinedScrutinee[Scrutinee, Result] = new RefinedScrutinee(result)
  def noMatch[Scrutinee <: Singleton, Result]: RefinedScrutinee[Scrutinee, Result] = new RefinedScrutinee(NoResult)
}
