
trait BigDecimalNewType:
  opaque type Type = BigDecimal
  def apply(value: BigDecimal): Type = value
  extension (self: Type)
    def value: BigDecimal = self
    inline def +(y: Type): Type = apply(self.value + y.value)

object Amount extends BigDecimalNewType {
  val x = Amount(0)
  val y = (x + x) + x
}