type Amount = Amount.Type
object Amount:
  opaque type Type = BigDecimal
  inline def apply(inline dec: BigDecimal): Type = dec

  extension (self: Type)
    inline def value: BigDecimal = self
    inline def +(y: Type): Type = self + y

@main def r(): Unit =
  val aa: Amount = Amount(1)
  val ab: Amount = Amount(2)
  val ac: Amount = Amount(2)
  val as1: Amount = aa + ab  // error
  val as2: Amount = aa + ab + ac // error

  println(s"aa + ab = ${as1}")
  println(s"aa + ab = ${as2}")
