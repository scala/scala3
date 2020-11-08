trait Semigroup[T] {
  extension (lhs: T) def append(rhs: T): T
}

object Semigroup {
  implicit object stringAppend extends Semigroup[String] {
    extension (lhs: String) override def append(rhs: String): String = lhs + rhs
  }

  implicit def sumSemigroup[N](implicit N: Numeric[N]): Semigroup[N] = new {
    extension (lhs: N) override def append(rhs: N): N = ??? // N.plus(lhs, rhs)
  }

  implicit class SumSemiGroupDeco[N](implicit N: Numeric[N]) extends Semigroup[N] {
    extension (lhs: N) override def append(rhs: N): N = ??? // N.plus(lhs, rhs)
  }
}

object Main {
  import Semigroup.sumSemigroup // this is not sufficient
  def f1 = {
    import Semigroup.stringAppend // necessary to make the extension method visible
    println("Hi" append " mum")
    println(1 append 2)
  }

  def f2 = {
    implicit val intSumAppend: Semigroup[Int] = sumSemigroup[Int]
    println(3 append 4)
  }

  def f3 = {
    import Semigroup.SumSemiGroupDeco
    sumSemigroup.append(1)(2)
  }
}