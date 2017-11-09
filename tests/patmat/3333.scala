sealed trait IntegralNumber

object IntegralNumber {
  object NaN extends IntegralNumber
  object NotNaN extends IntegralNumber

  sealed abstract class FiniteNumberImpl[N](val value: N) extends IntegralNumber
  sealed class IntNumber(value: Int) extends FiniteNumberImpl[Int](value)

  def test(o: IntegralNumber) = o match {
    case NaN => -1
  }

}
