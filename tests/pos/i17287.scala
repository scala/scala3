// https://github.com/scala/scala3/issues/17287
object Test17287:
  opaque type ConstrainedBigDecimal = BigDecimal

  extension (n: ConstrainedBigDecimal)
    inline def foo = n.isWhole

import Test17287.*

trait Quantity:
  type QuantityType <: ConstrainedBigDecimal

  def quantity: QuantityType

  def formatQuantity: String =
    if quantity.foo then quantity.toString() else f"$quantity%.2f"
