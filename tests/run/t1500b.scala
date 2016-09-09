sealed trait Nat
sealed trait Succ[Prev <: Nat] extends Nat
sealed trait Zero extends Nat

case class ToInt[N <: Nat](value: Int)

object ToInt {
  implicit val caseZero: ToInt[Zero] = ToInt(0)
  implicit def caseSucc[Prev <: Nat](implicit e: ToInt[Prev]): ToInt[Succ[Prev]] = ToInt(e.value + 1)
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(implicitly[ToInt[Zero]].value == 0)
    assert(implicitly[ToInt[Succ[Zero]]].value == 1)
    assert(implicitly[ToInt[Succ[Succ[Zero]]]].value == 2)
    assert(implicitly[ToInt[Succ[Succ[Succ[Zero]]]]].value == 3)
    assert(implicitly[ToInt[Succ[Succ[Succ[Succ[Zero]]]]]].value == 4)
    assert(implicitly[ToInt[Succ[Succ[Succ[Succ[Succ[Zero]]]]]]].value == 5)
  }
}
