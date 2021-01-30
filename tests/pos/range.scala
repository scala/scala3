import scala.math.*
import collection.immutable.NumericRange
object Test {
  val r1: scala.collection.immutable.Range.Partial[_, _] = ???
  val r2: scala.Range.Partial[_, _] = r1
  def until(d: BigDecimal, end: BigDecimal): Range.Partial[BigDecimal, NumericRange.Exclusive[BigDecimal]] =
    new Range.Partial(until(d, end, _))
  def until(d: BigDecimal, end: BigDecimal, step: BigDecimal) = Range.BigDecimal(d, end, step)
}
