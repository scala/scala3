opaque type PositiveNumber = Double

object PositiveNumber:
  extension (x: PositiveNumber)
    def mult1(other: PositiveNumber): PositiveNumber =
      x * other

extension (x: PositiveNumber)
  def mult2(other: PositiveNumber): PositiveNumber =
    x * other

object Test:
  def multMap1[A](x: Map[A, PositiveNumber], num: PositiveNumber): Map[A, PositiveNumber] = x.map((key, value) => key -> value.mult1(num)).toMap

  def multMap2[A](x: Map[A, PositiveNumber], num: PositiveNumber): Map[A, PositiveNumber] = x.map((key, value) => key -> value.mult2(num)).toMap // was error
//                                                                                                                                              ^
//                                             Cannot prove that (A, Double) <:< (A, V2).
//
//                                             where:    V2 is a type variable with constraint <: PositiveNumber
  def multMap2_2[A](x: Map[A, PositiveNumber], num: PositiveNumber): Map[A, PositiveNumber] = x.map((key, value) => key -> mult2(value)(num)).toMap
