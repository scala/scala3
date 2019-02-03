trait Semigroup[T] {
  def (lhs: T) append (rhs: T): T
  def (lhs: Int) appendS (rhs: T): T = ???
}

object Semigroup {
  implicit object stringAppend extends Semigroup[String] {
    override def (lhs: String) append (rhs: String): String = lhs + rhs
  }

  implicit def sumSemigroup[N](implicit N: Numeric[N]): Semigroup[N] = new {
    override def (lhs: N) append (rhs: N): N = N.plus(lhs, rhs)
    def (lhs: Int) appendS (rhs: N): N = ??? // N.plus(lhs, rhs)
  }
}


object Main {
  import Semigroup.sumSemigroup // this is not sufficient
  def f1 = {
    println(1 appendS 2) // error This should give the following error message:
/*
21 |    println(1 appendS 2)
   |            ^^^^^^^^^
   |value appendS is not a member of Int.
   |An extension method was tried, but could not be fully constructed:
   |
   |    Semigroup.sumSemigroup[Any](/* ambiguous */implicitly[Numeric[Any]]).appendS()
one error found
*/
  }
}