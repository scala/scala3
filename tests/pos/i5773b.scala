trait Semigroup[T] {
  extension (lhs: T) def append(rhs: T): T
  extension (lhs: Int) def appendS(rhs: T): T = ???
}

object Semigroup {
  implicit object stringAppend extends Semigroup[String] {
    extension (lhs: String) override def append(rhs: String): String = lhs + rhs
  }

  implicit def sumSemigroup[N](implicit N: Numeric[N]): Semigroup[N] = new {
    extension (lhs: N) override def append(rhs: N): N = N.plus(lhs, rhs)
    extension (lhs: Int) override def appendS(rhs: N): N = ??? // N.plus(lhs, rhs)
  }
}


object Main {
  import Semigroup.sumSemigroup // this is not sufficient
  def f1 = {
    println(1 appendS 2) // This used to give the following error message:
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