package scala.quoted

/** A type class for types that can be turned from a `quoted.Expr[T]` to a `T` */
trait Unliftable[T] {

  /** Return the value of the expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  def fromExpr(x: Expr[T]): Quotes ?=> Option[T]

}

object Unliftable {

  /** Default unliftable for Unit */
  given UnitUnliftable as Unliftable[Unit] = new PrimitiveUnliftable

  /** Default unliftable for Boolean */
  given BooleanUnliftable as Unliftable[Boolean] = new PrimitiveUnliftable

  /** Default unliftable for Byte */
  given ByteUnliftable as Unliftable[Byte] = new PrimitiveUnliftable

  /** Default unliftable for Short */
  given ShortUnliftable as Unliftable[Short] = new PrimitiveUnliftable

  /** Default unliftable for Int */
  given IntUnliftable as Unliftable[Int] = new PrimitiveUnliftable

  /** Default unliftable for Long */
  given LongUnliftable as Unliftable[Long] = new PrimitiveUnliftable

  /** Default unliftable for Float */
  given FloatUnliftable as Unliftable[Float] = new PrimitiveUnliftable

  /** Default unliftable for Double */
  given DoubleUnliftable as Unliftable[Double] = new PrimitiveUnliftable

  /** Default unliftable for Char */
  given CharUnliftable as Unliftable[Char] = new PrimitiveUnliftable

  /** Default unliftable for String */
  given StringUnliftable as Unliftable[String] = new PrimitiveUnliftable

  private class PrimitiveUnliftable[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends Unliftable[T] {
    /** Lift a quoted primitive value `'{ n }` into `n` */
    def fromExpr(x: Expr[T]) = Const.unapply(x)
  }

  /** Default unliftable for Option */
  given OptionUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Option[T]] = new {
    def fromExpr(x: Expr[Option[T]]) = x match {
      case '{ Option[T](${Unlifted(y)}) } => Some(Option(y))
      case '{ None } => Some(None)
      case '{ ${Unlifted(opt)} : Some[T] } => Some(opt)
      case _ => None
    }
  }

  /** Default unliftable for None */
  given NoneUnliftable as Unliftable[None.type] = new {
    def fromExpr(x: Expr[None.type]) = x match {
      case '{ None } => Some(None)
      case _ => None
    }
  }

  /** Default unliftable for Some */
  given SomeUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Some[T]] = new {
    def fromExpr(x: Expr[Some[T]]) = x match {
      case '{ new Some[T](${Unlifted(y)}) } => Some(Some(y))
      case '{     Some[T](${Unlifted(y)}) } => Some(Some(y))
      case _ => None
    }
  }

  /** Default unliftable for StringContext */
  given StringContextUnliftable as Unliftable[StringContext] = new {
    def fromExpr(x: Expr[StringContext]) = x match {
      case '{ new StringContext(${Varargs(Consts(args))}: _*) } => Some(StringContext(args: _*))
      case '{     StringContext(${Varargs(Consts(args))}: _*) } => Some(StringContext(args: _*))
      case _ => None
    }
  }

  /** Default unliftable for EmptyTuple */
  given EmptyTupleUnliftable as Unliftable[EmptyTuple.type] = new {
    def fromExpr(x: Expr[EmptyTuple.type]) = x match {
      case '{ EmptyTuple } => Some(EmptyTuple)
      case _ => None
    }
  }

  /** Default unliftable for Tuple1 */
  given Tuple1Unliftable[T1](using Type[T1], Unliftable[T1]) as Unliftable[Tuple1[T1]] = new {
    def fromExpr(x: Expr[Tuple1[T1]]) = x match {
      case '{ new Tuple1[T1](${Unlifted(y)}) } => Some(Tuple1(y))
      case '{     Tuple1[T1](${Unlifted(y)}) } => Some(Tuple1(y))
      case _ => None
    }
  }

  /** Default unliftable for Tuple2 */
  given Tuple2Unliftable[T1, T2](using Type[T1], Type[T2], Unliftable[T1], Unliftable[T2]) as Unliftable[Tuple2[T1, T2]] = new {
    def fromExpr(x: Expr[Tuple2[T1, T2]]) = x match {
      case '{ new Tuple2[T1, T2](${Unlifted(y1)}, ${Unlifted(y2)}) } => Some(Tuple2(y1, y2))
      case '{     Tuple2[T1, T2](${Unlifted(y1)}, ${Unlifted(y2)}) } => Some(Tuple2(y1, y2))
      case '{ (${Unlifted(y1)}: T1) -> (${Unlifted(y2)}: T2) } => Some(Tuple2(y1, y2))
      case _ => None
    }
  }

  /** Default unliftable for Tuple3 */
  given Tuple3Unliftable[T1, T2, T3](using Type[T1], Type[T2], Type[T3], Unliftable[T1], Unliftable[T2], Unliftable[T3]) as Unliftable[Tuple3[T1, T2, T3]] = new {
    def fromExpr(x: Expr[Tuple3[T1, T2, T3]]) = x match {
      case '{ new Tuple3[T1, T2, T3](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}) } => Some(Tuple3(y1, y2, y3))
      case '{     Tuple3[T1, T2, T3](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}) } => Some(Tuple3(y1, y2, y3))
      case _ => None
    }
  }

  /** Default unliftable for Tuple4 */
  given Tuple4Unliftable[T1, T2, T3, T4](using Type[T1], Type[T2], Type[T3], Type[T4], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4]) as Unliftable[Tuple4[T1, T2, T3, T4]] = new {
    def fromExpr(x: Expr[Tuple4[T1, T2, T3, T4]]) = x match {
      case '{ new Tuple4[T1, T2, T3, T4](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case '{     Tuple4[T1, T2, T3, T4](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case _ => None
    }
  }

  /** Default unliftable for Tuple5 */
  given Tuple5Unliftable[T1, T2, T3, T4, T5](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5]) as Unliftable[Tuple5[T1, T2, T3, T4, T5]] = new {
    def fromExpr(x: Expr[Tuple5[T1, T2, T3, T4, T5]]) = x match {
      case '{ new Tuple5[T1, T2, T3, T4, T5](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case '{     Tuple5[T1, T2, T3, T4, T5](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case _ => None
    }
  }

  /** Default unliftable for Tuple6 */
  given Tuple6Unliftable[T1, T2, T3, T4, T5, T6](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6]) as Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def fromExpr(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]]) = x match {
      case '{ new Tuple6[T1, T2, T3, T4, T5, T6](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case '{     Tuple6[T1, T2, T3, T4, T5, T6](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case _ => None
    }
  }

  /** Default unliftable for Tuple7 */
  given Tuple7Unliftable[T1, T2, T3, T4, T5, T6, T7](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7]) as Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def fromExpr(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]) = x match {
      case '{ new Tuple7[T1, T2, T3, T4, T5, T6, T7](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case '{     Tuple7[T1, T2, T3, T4, T5, T6, T7](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case _ => None
    }
  }

  /** Default unliftable for Tuple8 */
  given Tuple8Unliftable[T1, T2, T3, T4, T5, T6, T7, T8](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8]) as Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def fromExpr(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]) = x match {
      case '{ new Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case '{     Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case _ => None
    }
  }

  /** Default unliftable for Tuple9 */
  given Tuple9Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9]) as Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def fromExpr(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]) = x match {
      case '{ new Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case '{     Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case _ => None
    }
  }

  /** Default unliftable for Tuple10 */
  given Tuple10Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10]) as Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def fromExpr(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]) = x match {
      case '{ new Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case '{     Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case _ => None
    }
  }

  /** Default unliftable for Tuple11 */
  given Tuple11Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11]) as Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def fromExpr(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]) = x match {
      case '{ new Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case '{     Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case _ => None
    }
  }

  /** Default unliftable for Tuple12 */
  given Tuple12Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12]) as Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def fromExpr(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]) = x match {
      case '{ new Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case '{     Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case _ => None
    }
  }

  /** Default unliftable for Tuple13 */
  given Tuple13Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13]) as Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def fromExpr(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]) = x match {
      case '{ new Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case '{     Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case _ => None
    }
  }

  /** Default unliftable for Tuple14 */
  given Tuple14Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14]) as Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def fromExpr(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]) = x match {
      case '{ new Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case '{     Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case _ => None
    }
  }

  /** Default unliftable for Tuple15 */
  given Tuple15Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15]) as Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def fromExpr(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]) = x match {
      case '{ new Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case '{     Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case _ => None
    }
  }

  /** Default unliftable for Tuple16 */
  given Tuple16Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16]) as Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def fromExpr(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]) = x match {
      case '{ new Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case '{     Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case _ => None
    }
  }

  /** Default unliftable for Tuple17 */
  given Tuple17Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17]) as Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def fromExpr(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]) = x match {
      case '{ new Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case '{     Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case _ => None
    }
  }

  /** Default unliftable for Tuple18 */
  given Tuple18Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18]) as Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def fromExpr(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]) = x match {
      case '{ new Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case '{     Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case _ => None
    }
  }

  /** Default unliftable for Tuple19 */
  given Tuple19Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19]) as Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def fromExpr(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) = x match {
      case '{ new Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case '{     Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case _ => None
    }
  }

  /** Default unliftable for Tuple20 */
  given Tuple20Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20]) as Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def fromExpr(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]) = x match {
      case '{ new Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case '{     Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case _ => None
    }
  }

  /** Default unliftable for Tuple21 */
  given Tuple21Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20], Unliftable[T21]) as Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def fromExpr(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]) = x match {
      case '{ new Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case '{     Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case _ => None
    }
  }

  /** Default unliftable for Tuple22 */
  given Tuple22Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20], Unliftable[T21], Unliftable[T22]) as Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def fromExpr(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]) = x match {
      case '{ new Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}, ${Unlifted(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case '{     Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}, ${Unlifted(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case _ => None
    }
  }

  /** Default unliftable for Seq */
  given SeqUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Seq[T]] = new {
    def fromExpr(x: Expr[Seq[T]]) = x match {
      case Varargs(Unlifted(elems)) => Some(elems)
      case '{ scala.Seq[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems)
      case '{ scala.collection.immutable.Seq[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems)
      case '{  ${Unlifted(x)}: List[T] } => Some(x)
      case _ => None
    }
  }

  /** Default unliftable for Nil */
  given NilUnliftable as Unliftable[Nil.type] = new {
    def fromExpr(x: Expr[Nil.type]) = x match {
      case '{ scala.Nil } |  '{ scala.collection.immutable.Nil } => Some(Nil)
      case _ => None
    }
  }

  /** Default unliftable for List */
  given ListUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[List[T]] = new {
    def fromExpr(x: Expr[List[T]]) = x match {
      case '{ scala.List[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toList)
      case '{ scala.List.empty[T] } => Some(Nil)
      case '{ scala.collection.immutable.List[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toList)
      case '{ scala.collection.immutable.List.empty[T] } => Some(Nil)
      case _ => None
    }
  }

  /** Default unliftable for Set */
  given SetUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Set[T]] = new {
    def fromExpr(x: Expr[Set[T]]) = x match {
      case '{ Set[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toSet)
      case '{ Set.empty[T] } => Some(Set.empty[T])
      case '{ scala.collection.immutable.Set[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toSet)
      case '{ scala.collection.immutable.Set.empty[T] } => Some(Set.empty[T])
      case _ => None
    }
  }

  /** Default unliftable for Map */
  given MapUnliftable[T, U](using Type[T], Type[U], Unliftable[T], Unliftable[U]) as Unliftable[Map[T, U]] = new {
    def fromExpr(x: Expr[Map[T, U]]) = x match {
      case '{ Map[T, U](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toMap)
      case '{ Map.empty[T, U] } => Some(Map.empty)
      case '{ scala.collection.immutable.Map[T, U](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toMap)
      case '{ scala.collection.immutable.Map.empty[T, U] } => Some(Map.empty)
      case _ => None
    }
  }

  /** Default unliftable for Either */
  given EitherUnliftable[L, R](using Type[L], Type[R], Unliftable[L], Unliftable[R]) as Unliftable[Either[L, R]] = new {
    def fromExpr(x: Expr[Either[L, R]]) = x match {
      case '{ $x: Left[L, R] } => x.unlift
      case '{ $x: Right[L, R] } => x.unlift
      case _ => None
    }
  }

  /** Default unliftable for Left */
  given LeftUnliftable[L, R](using Type[L], Type[R], Unliftable[L]) as Unliftable[Left[L, R]] = new {
    def fromExpr(x: Expr[Left[L, R]]) = x match {
      case '{ Left[L, R](${Unlifted(x)}) } => Some(Left(x))
      case _ => None
    }
  }

  /** Default unliftable for Right */
  given RightUnliftable[L, R](using Type[L], Type[R], Unliftable[R]) as Unliftable[Right[L, R]] = new {
    def fromExpr(x: Expr[Right[L, R]]) = x match {
      case '{ Right[L, R](${Unlifted(x)}) } => Some(Right(x))
      case _ => None
    }
  }

}
