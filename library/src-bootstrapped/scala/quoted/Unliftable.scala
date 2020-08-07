package scala.quoted

/** A type class for types that can be turned from a `quoted.Expr[T]` to a `T` */
trait Unliftable[T] {

  /** Return the value of the expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  def apply(x: Expr[T])(using qctx: QuoteContext): Option[T]

}

object Unliftable {

  given Unliftable_UnitIsUnliftable as Unliftable[Unit] = new PrimitiveUnliftable
  given Unliftable_BooleanIsUnliftable as Unliftable[Boolean] = new PrimitiveUnliftable
  given Unliftable_ByteIsUnliftable as Unliftable[Byte] = new PrimitiveUnliftable
  given Unliftable_ShortIsUnliftable as Unliftable[Short] = new PrimitiveUnliftable
  given Unliftable_IntIsUnliftable as Unliftable[Int] = new PrimitiveUnliftable
  given Unliftable_LongIsUnliftable as Unliftable[Long] = new PrimitiveUnliftable
  given Unliftable_FloatIsUnliftable as Unliftable[Float] = new PrimitiveUnliftable
  given Unliftable_DoubleIsUnliftable as Unliftable[Double] = new PrimitiveUnliftable
  given Unliftable_CharIsUnliftable as Unliftable[Char] = new PrimitiveUnliftable
  given Unliftable_StringIsUnliftable as Unliftable[String] = new PrimitiveUnliftable

  private class PrimitiveUnliftable[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends Unliftable[T] {
    /** Lift a quoted primitive value `'{ n }` into `n` */
    def apply(x: Expr[T])(using qctx: QuoteContext): Option[T] = Const.unapply(x)
  }

  given OptionIsUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Option[T]] = new {
    def apply(x: Expr[Option[T]])(using qctx: QuoteContext): Option[Option[T]] = x match {
      case '{ Option[T](${Unlifted(y)}) } => Some(Option(y))
      case '{ None } => Some(None)
      case '{ ${Unlifted(opt)} : Some[T] } => Some(opt)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.OptionIsUnliftable"
  }

  given NoneIsUnliftable as Unliftable[None.type] = new {
    def apply(x: Expr[None.type])(using qctx: QuoteContext): Option[None.type] = x match {
      case '{ None } => Some(None)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.NoneIsUnliftable"
  }

  given SomeIsUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Some[T]] = new {
    def apply(x: Expr[Some[T]])(using qctx: QuoteContext): Option[Some[T]] = x match {
      case '{ new Some[T](${Unlifted(y)}) } => Some(Some(y))
      case '{     Some[T](${Unlifted(y)}) } => Some(Some(y))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.SomeIsUnliftable"
  }

  given StringContextIsUnliftable as Unliftable[StringContext] = new {
    def apply(x: Expr[StringContext])(using qctx: QuoteContext): Option[StringContext] = x match {
      case '{ new StringContext(${Varargs(Consts(args))}: _*) } => Some(StringContext(args: _*))
      case '{     StringContext(${Varargs(Consts(args))}: _*) } => Some(StringContext(args: _*))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple1IsUnliftable"
  }

  given EmptyTupleIsUnliftable as Unliftable[EmptyTuple.type] = new {
    def apply(x: Expr[EmptyTuple.type])(using qctx: QuoteContext): Option[EmptyTuple.type] = x match {
      case '{ EmptyTuple } => Some(EmptyTuple)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple1IsUnliftable"
  }

  given Tuple1IsUnliftable[T1](using Type[T1], Unliftable[T1]) as Unliftable[Tuple1[T1]] = new {
    def apply(x: Expr[Tuple1[T1]])(using qctx: QuoteContext): Option[Tuple1[T1]] = x match {
      case '{ new Tuple1[T1](${Unlifted(y)}) } => Some(Tuple1(y))
      case '{     Tuple1[T1](${Unlifted(y)}) } => Some(Tuple1(y))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple1IsUnliftable"
  }

  given Tuple2IsUnliftable[T1, T2](using Type[T1], Type[T2], Unliftable[T1], Unliftable[T2]) as Unliftable[Tuple2[T1, T2]] = new {
    def apply(x: Expr[Tuple2[T1, T2]])(using qctx: QuoteContext): Option[Tuple2[T1, T2]] = x match {
      case '{ new Tuple2[T1, T2](${Unlifted(y1)}, ${Unlifted(y2)}) } => Some(Tuple2(y1, y2))
      case '{     Tuple2[T1, T2](${Unlifted(y1)}, ${Unlifted(y2)}) } => Some(Tuple2(y1, y2))
      case '{ (${Unlifted(y1)}: T1) -> (${Unlifted(y2)}: T2) } => Some(Tuple2(y1, y2))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple2IsUnliftable"
  }


  given Tuple3IsUnliftable[T1, T2, T3](using Type[T1], Type[T2], Type[T3], Unliftable[T1], Unliftable[T2], Unliftable[T3]) as Unliftable[Tuple3[T1, T2, T3]] = new {
    def apply(x: Expr[Tuple3[T1, T2, T3]])(using qctx: QuoteContext): Option[Tuple3[T1, T2, T3]] = x match {
      case '{ new Tuple3[T1, T2, T3](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}) } => Some(Tuple3(y1, y2, y3))
      case '{     Tuple3[T1, T2, T3](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}) } => Some(Tuple3(y1, y2, y3))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple3IsUnliftable"
  }


  given Tuple4IsUnliftable[T1, T2, T3, T4](using Type[T1], Type[T2], Type[T3], Type[T4], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4]) as Unliftable[Tuple4[T1, T2, T3, T4]] = new {
    def apply(x: Expr[Tuple4[T1, T2, T3, T4]])(using qctx: QuoteContext): Option[Tuple4[T1, T2, T3, T4]] = x match {
      case '{ new Tuple4[T1, T2, T3, T4](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case '{     Tuple4[T1, T2, T3, T4](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple4IsUnliftable"
  }


  given Tuple5IsUnliftable[T1, T2, T3, T4, T5](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5]) as Unliftable[Tuple5[T1, T2, T3, T4, T5]] = new {
    def apply(x: Expr[Tuple5[T1, T2, T3, T4, T5]])(using qctx: QuoteContext): Option[Tuple5[T1, T2, T3, T4, T5]] = x match {
      case '{ new Tuple5[T1, T2, T3, T4, T5](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case '{     Tuple5[T1, T2, T3, T4, T5](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple5IsUnliftable"
  }


  given Tuple6IsUnliftable[T1, T2, T3, T4, T5, T6](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6]) as Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def apply(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]])(using qctx: QuoteContext): Option[Tuple6[T1, T2, T3, T4, T5, T6]] = x match {
      case '{ new Tuple6[T1, T2, T3, T4, T5, T6](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case '{     Tuple6[T1, T2, T3, T4, T5, T6](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple6IsUnliftable"
  }


  given Tuple7IsUnliftable[T1, T2, T3, T4, T5, T6, T7](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7]) as Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def apply(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]])(using qctx: QuoteContext): Option[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = x match {
      case '{ new Tuple7[T1, T2, T3, T4, T5, T6, T7](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case '{     Tuple7[T1, T2, T3, T4, T5, T6, T7](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple7IsUnliftable"
  }


  given Tuple8IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8]) as Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def apply(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]])(using qctx: QuoteContext): Option[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = x match {
      case '{ new Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case '{     Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple8IsUnliftable"
  }


  given Tuple9IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9]) as Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def apply(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]])(using qctx: QuoteContext): Option[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = x match {
      case '{ new Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case '{     Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple9IsUnliftable"
  }


  given Tuple10IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10]) as Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def apply(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]])(using qctx: QuoteContext): Option[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = x match {
      case '{ new Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case '{     Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple10IsUnliftable"
  }


  given Tuple11IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11]) as Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def apply(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]])(using qctx: QuoteContext): Option[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = x match {
      case '{ new Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case '{     Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple11IsUnliftable"
  }


  given Tuple12IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12]) as Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def apply(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]])(using qctx: QuoteContext): Option[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = x match {
      case '{ new Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case '{     Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple12IsUnliftable"
  }


  given Tuple13IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13]) as Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def apply(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]])(using qctx: QuoteContext): Option[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = x match {
      case '{ new Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case '{     Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple13IsUnliftable"
  }


  given Tuple14IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14]) as Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def apply(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]])(using qctx: QuoteContext): Option[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = x match {
      case '{ new Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case '{     Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple14IsUnliftable"
  }


  given Tuple15IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15]) as Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def apply(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]])(using qctx: QuoteContext): Option[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = x match {
      case '{ new Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case '{     Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple15IsUnliftable"
  }


  given Tuple16IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16]) as Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def apply(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]])(using qctx: QuoteContext): Option[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = x match {
      case '{ new Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case '{     Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple16IsUnliftable"
  }


  given Tuple17IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17]) as Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def apply(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]])(using qctx: QuoteContext): Option[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = x match {
      case '{ new Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case '{     Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple17IsUnliftable"
  }


  given Tuple18IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18]) as Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def apply(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]])(using qctx: QuoteContext): Option[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = x match {
      case '{ new Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case '{     Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple18IsUnliftable"
  }


  given Tuple19IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19]) as Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def apply(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]])(using qctx: QuoteContext): Option[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = x match {
      case '{ new Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case '{     Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple19IsUnliftable"
  }


  given Tuple20IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20]) as Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def apply(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]])(using qctx: QuoteContext): Option[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = x match {
      case '{ new Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case '{     Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple20IsUnliftable"
  }


  given Tuple21IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20], Unliftable[T21]) as Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def apply(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]])(using qctx: QuoteContext): Option[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = x match {
      case '{ new Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case '{     Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple21IsUnliftable"
  }


  given Tuple22IsUnliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20], Unliftable[T21], Unliftable[T22]) as Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def apply(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]])(using qctx: QuoteContext): Option[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = x match {
      case '{ new Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}, ${Unlifted(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case '{     Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Unlifted(y1)}, ${Unlifted(y2)}, ${Unlifted(y3)}, ${Unlifted(y4)}, ${Unlifted(y5)}, ${Unlifted(y6)}, ${Unlifted(y7)}, ${Unlifted(y8)}, ${Unlifted(y9)}, ${Unlifted(y10)}, ${Unlifted(y11)}, ${Unlifted(y12)}, ${Unlifted(y13)}, ${Unlifted(y14)}, ${Unlifted(y15)}, ${Unlifted(y16)}, ${Unlifted(y17)}, ${Unlifted(y18)}, ${Unlifted(y19)}, ${Unlifted(y20)}, ${Unlifted(y21)}, ${Unlifted(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.Tuple22IsUnliftable"
  }

  given SeqIsUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Seq[T]] = new {
    def apply(x: Expr[Seq[T]])(using qctx: QuoteContext): Option[Seq[T]] = x match {
      case Varargs(Unlifted(elems)) => Some(elems)
      case '{ scala.Seq[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems)
      case '{ scala.collection.immutable.Seq[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems)
      case '{  ${Unlifted(x)}: List[T] } => Some(x)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.SeqIsUnliftable"
  }

  given NilIsUnliftable as Unliftable[Nil.type] = new {
    def apply(x: Expr[Nil.type])(using qctx: QuoteContext): Option[Nil.type] = x match {
      case '{ scala.Nil } |  '{ scala.collection.immutable.Nil } => Some(Nil)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.NilIsUnliftable"
  }

  given ListIsUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[List[T]] = new {
    def apply(x: Expr[List[T]])(using qctx: QuoteContext): Option[List[T]] = x match {
      case '{ scala.List[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toList)
      case '{ scala.List.empty[T] } => Some(Nil)
      case '{ scala.collection.immutable.List[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toList)
      case '{ scala.collection.immutable.List.empty[T] } => Some(Nil)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.SeqIsUnliftable"
  }


  given SetIsUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Set[T]] = new {
    def apply(x: Expr[Set[T]])(using qctx: QuoteContext): Option[Set[T]] = x match {
      case '{ Set[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toSet)
      case '{ Set.empty[T] } => Some(Set.empty[T])
      case '{ scala.collection.immutable.Set[T](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toSet)
      case '{ scala.collection.immutable.Set.empty[T] } => Some(Set.empty[T])
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.SetIsUnliftable"
  }


  given MapIsUnliftable[T, U](using Type[T], Type[U], Unliftable[T], Unliftable[U]) as Unliftable[Map[T, U]] = new {
    def apply(x: Expr[Map[T, U]])(using qctx: QuoteContext): Option[Map[T, U]] = x match {
      case '{ Map[T, U](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toMap)
      case '{ Map.empty[T, U] } => Some(Map.empty)
      case '{ scala.collection.immutable.Map[T, U](${Varargs(Unlifted(elems))}: _*) } => Some(elems.toMap)
      case '{ scala.collection.immutable.Map.empty[T, U] } => Some(Map.empty)
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.MapIsUnliftable"
  }

  given EitherIsUnliftable[L, R](using Type[L], Type[R], Unliftable[L], Unliftable[R]) as Unliftable[Either[L, R]] = new {
    def apply(x: Expr[Either[L, R]])(using qctx: QuoteContext): Option[Either[L, R]] = x match {
      case '{ $x: Left[L, R] } => x.unlift
      case '{ $x: Right[L, R] } => x.unlift
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.EitherIsUnliftable"
  }

  given LeftIsUnliftable[L, R](using Type[L], Type[R], Unliftable[L]) as Unliftable[Left[L, R]] = new {
    def apply(x: Expr[Left[L, R]])(using qctx: QuoteContext): Option[Left[L, R]] = x match {
      case '{ Left[L, R](${Unlifted(x)}) } => Some(Left(x))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.LeftIsUnliftable"
  }

  given RightIsUnliftable[L, R](using Type[L], Type[R], Unliftable[R]) as Unliftable[Right[L, R]] = new {
    def apply(x: Expr[Right[L, R]])(using qctx: QuoteContext): Option[Right[L, R]] = x match {
      case '{ Right[L, R](${Unlifted(x)}) } => Some(Right(x))
      case _ => None
    }
    override def toString(): String = "scala.quoted.Unliftable.RightIsUnliftable"
  }

}
