sealed trait IntegralNumber
sealed trait FiniteNumber extends IntegralNumber

object IntegralNumber {

  sealed abstract class BaseNumber extends IntegralNumber
  sealed abstract class NonFinite extends BaseNumber
  object NaN extends NonFinite
  sealed abstract class FiniteNumberImpl[N](val value: N) extends BaseNumber with FiniteNumber
  sealed class IntNumber(value: Int) extends FiniteNumberImpl[Int](value)

  def test(t: IntNumber, o: IntegralNumber) = o match {
    case NaN => -1
    case o: IntNumber => t.value.compare(o.value)
  }

}