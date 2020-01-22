package scala.quoted

import scala.quoted.matching._

/** A typeclass for types that can be turned from a `quoted.Expr[T]` to a `T` */
trait ValueOfExpr[T] {

  /** Return the value of the expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  def apply(x: Expr[T])(given QuoteContext): Option[T]

}

object ValueOfExpr {

  given ValueOfExpr_Unit_delegate: ValueOfExpr[Unit] = new PrimitiveValueOfExpr
  given ValueOfExpr_Boolean_delegate: ValueOfExpr[Boolean] = new PrimitiveValueOfExpr
  given ValueOfExpr_Byte_delegate: ValueOfExpr[Byte] = new PrimitiveValueOfExpr
  given ValueOfExpr_Short_delegate: ValueOfExpr[Short] = new PrimitiveValueOfExpr
  given ValueOfExpr_Int_delegate: ValueOfExpr[Int] = new PrimitiveValueOfExpr
  given ValueOfExpr_Long_delegate: ValueOfExpr[Long] = new PrimitiveValueOfExpr
  given ValueOfExpr_Float_delegate: ValueOfExpr[Float] = new PrimitiveValueOfExpr
  given ValueOfExpr_Double_delegate: ValueOfExpr[Double] = new PrimitiveValueOfExpr
  given ValueOfExpr_Char_delegate: ValueOfExpr[Char] = new PrimitiveValueOfExpr
  given ValueOfExpr_String_delegate: ValueOfExpr[String] = new PrimitiveValueOfExpr

  private class PrimitiveValueOfExpr[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends ValueOfExpr[T] {
    /** Lift a quoted primitive value `'{ n }` into `n` */
    def apply(x: Expr[T])(given QuoteContext): Option[T] = matching.Const.unapply(x)
  }

  given Option_delegate[T](given Type[T], ValueOfExpr[T]): ValueOfExpr[Option[T]] = new {
    def apply(x: Expr[Option[T]])(given QuoteContext): Option[Option[T]] = x match {
      case '{ None: Option[T] } => Some(None) // FIXME: remove ascription, Matcher should be able to match this expression
      case '{ new Some[T](${Value(y)}) } => Some(Some(y))
      case '{     Some[T](${Value(y)}) } => Some(Some(y))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Option_delegate"
  }

  given StringContext_delegate: ValueOfExpr[StringContext] = new {
    def apply(x: Expr[StringContext])(given QuoteContext): Option[StringContext] = x match {
      case '{ new StringContext(${ConstSeq(args)}: _*) } => Some(StringContext(args: _*))
      case '{     StringContext(${ConstSeq(args)}: _*) } => Some(StringContext(args: _*))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple1_delegate"
  }


  given Tuple1_delegate[T1](given Type[T1], ValueOfExpr[T1]): ValueOfExpr[Tuple1[T1]] = new {
    def apply(x: Expr[Tuple1[T1]])(given QuoteContext): Option[Tuple1[T1]] = x match {
      case '{ new Tuple1[T1](${Value(y)}) } => Some(Tuple1(y))
      case '{     Tuple1[T1](${Value(y)}) } => Some(Tuple1(y))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple1_delegate"
  }

  given Tuple2_delegate[T1, T2](given Type[T1], Type[T2], ValueOfExpr[T1], ValueOfExpr[T2]): ValueOfExpr[Tuple2[T1, T2]] = new {
    def apply(x: Expr[Tuple2[T1, T2]])(given QuoteContext): Option[Tuple2[T1, T2]] = x match {
      case '{ new Tuple2[T1, T2](${Value(y1)}, ${Value(y2)}) } => Some(Tuple2(y1, y2))
      case '{     Tuple2[T1, T2](${Value(y1)}, ${Value(y2)}) } => Some(Tuple2(y1, y2))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple2_delegate"
  }


  given Tuple3_delegate[T1, T2, T3](given Type[T1], Type[T2], Type[T3], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3]): ValueOfExpr[Tuple3[T1, T2, T3]] = new {
    def apply(x: Expr[Tuple3[T1, T2, T3]])(given QuoteContext): Option[Tuple3[T1, T2, T3]] = x match {
      case '{ new Tuple3[T1, T2, T3](${Value(y1)}, ${Value(y2)}, ${Value(y3)}) } => Some(Tuple3(y1, y2, y3))
      case '{     Tuple3[T1, T2, T3](${Value(y1)}, ${Value(y2)}, ${Value(y3)}) } => Some(Tuple3(y1, y2, y3))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple3_delegate"
  }


  given Tuple4_delegate[T1, T2, T3, T4](given Type[T1], Type[T2], Type[T3], Type[T4], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4]): ValueOfExpr[Tuple4[T1, T2, T3, T4]] = new {
    def apply(x: Expr[Tuple4[T1, T2, T3, T4]])(given QuoteContext): Option[Tuple4[T1, T2, T3, T4]] = x match {
      case '{ new Tuple4[T1, T2, T3, T4](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case '{     Tuple4[T1, T2, T3, T4](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple4_delegate"
  }


  given Tuple5_delegate[T1, T2, T3, T4, T5](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5]): ValueOfExpr[Tuple5[T1, T2, T3, T4, T5]] = new {
    def apply(x: Expr[Tuple5[T1, T2, T3, T4, T5]])(given QuoteContext): Option[Tuple5[T1, T2, T3, T4, T5]] = x match {
      case '{ new Tuple5[T1, T2, T3, T4, T5](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case '{     Tuple5[T1, T2, T3, T4, T5](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple5_delegate"
  }


  given Tuple6_delegate[T1, T2, T3, T4, T5, T6](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6]): ValueOfExpr[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def apply(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]])(given QuoteContext): Option[Tuple6[T1, T2, T3, T4, T5, T6]] = x match {
      case '{ new Tuple6[T1, T2, T3, T4, T5, T6](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case '{     Tuple6[T1, T2, T3, T4, T5, T6](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple6_delegate"
  }


  given Tuple7_delegate[T1, T2, T3, T4, T5, T6, T7](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7]): ValueOfExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def apply(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]])(given QuoteContext): Option[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = x match {
      case '{ new Tuple7[T1, T2, T3, T4, T5, T6, T7](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case '{     Tuple7[T1, T2, T3, T4, T5, T6, T7](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple7_delegate"
  }


  given Tuple8_delegate[T1, T2, T3, T4, T5, T6, T7, T8](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8]): ValueOfExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def apply(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]])(given QuoteContext): Option[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = x match {
      case '{ new Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case '{     Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple8_delegate"
  }


  given Tuple9_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9]): ValueOfExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def apply(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]])(given QuoteContext): Option[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = x match {
      case '{ new Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case '{     Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple9_delegate"
  }


  given Tuple10_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10]): ValueOfExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def apply(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]])(given QuoteContext): Option[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = x match {
      case '{ new Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case '{     Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple10_delegate"
  }


  given Tuple11_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11]): ValueOfExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def apply(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]])(given QuoteContext): Option[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = x match {
      case '{ new Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case '{     Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple11_delegate"
  }


  given Tuple12_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12]): ValueOfExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def apply(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]])(given QuoteContext): Option[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = x match {
      case '{ new Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case '{     Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple12_delegate"
  }


  given Tuple13_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13]): ValueOfExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def apply(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]])(given QuoteContext): Option[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = x match {
      case '{ new Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case '{     Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple13_delegate"
  }


  given Tuple14_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14]): ValueOfExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def apply(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]])(given QuoteContext): Option[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = x match {
      case '{ new Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case '{     Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple14_delegate"
  }


  given Tuple15_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15]): ValueOfExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def apply(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]])(given QuoteContext): Option[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = x match {
      case '{ new Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case '{     Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple15_delegate"
  }


  given Tuple16_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16]): ValueOfExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def apply(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]])(given QuoteContext): Option[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = x match {
      case '{ new Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case '{     Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple16_delegate"
  }


  given Tuple17_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16], ValueOfExpr[T17]): ValueOfExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def apply(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]])(given QuoteContext): Option[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = x match {
      case '{ new Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case '{     Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple17_delegate"
  }


  given Tuple18_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16], ValueOfExpr[T17], ValueOfExpr[T18]): ValueOfExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def apply(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]])(given QuoteContext): Option[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = x match {
      case '{ new Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case '{     Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple18_delegate"
  }


  given Tuple19_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16], ValueOfExpr[T17], ValueOfExpr[T18], ValueOfExpr[T19]): ValueOfExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def apply(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]])(given QuoteContext): Option[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = x match {
      case '{ new Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case '{     Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple19_delegate"
  }


  given Tuple20_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16], ValueOfExpr[T17], ValueOfExpr[T18], ValueOfExpr[T19], ValueOfExpr[T20]): ValueOfExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def apply(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]])(given QuoteContext): Option[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = x match {
      case '{ new Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}, ${Value(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case '{     Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}, ${Value(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple20_delegate"
  }


  given Tuple21_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16], ValueOfExpr[T17], ValueOfExpr[T18], ValueOfExpr[T19], ValueOfExpr[T20], ValueOfExpr[T21]): ValueOfExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def apply(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]])(given QuoteContext): Option[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = x match {
      case '{ new Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}, ${Value(y20)}, ${Value(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case '{     Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}, ${Value(y20)}, ${Value(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple21_delegate"
  }


  given Tuple22_delegate[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](given Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], ValueOfExpr[T1], ValueOfExpr[T2], ValueOfExpr[T3], ValueOfExpr[T4], ValueOfExpr[T5], ValueOfExpr[T6], ValueOfExpr[T7], ValueOfExpr[T8], ValueOfExpr[T9], ValueOfExpr[T10], ValueOfExpr[T11], ValueOfExpr[T12], ValueOfExpr[T13], ValueOfExpr[T14], ValueOfExpr[T15], ValueOfExpr[T16], ValueOfExpr[T17], ValueOfExpr[T18], ValueOfExpr[T19], ValueOfExpr[T20], ValueOfExpr[T21], ValueOfExpr[T22]): ValueOfExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def apply(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]])(given QuoteContext): Option[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = x match {
      case '{ new Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}, ${Value(y20)}, ${Value(y21)}, ${Value(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case '{     Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Value(y1)}, ${Value(y2)}, ${Value(y3)}, ${Value(y4)}, ${Value(y5)}, ${Value(y6)}, ${Value(y7)}, ${Value(y8)}, ${Value(y9)}, ${Value(y10)}, ${Value(y11)}, ${Value(y12)}, ${Value(y13)}, ${Value(y14)}, ${Value(y15)}, ${Value(y16)}, ${Value(y17)}, ${Value(y18)}, ${Value(y19)}, ${Value(y20)}, ${Value(y21)}, ${Value(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Tuple22_delegate"
  }

  given Seq_delegate[T](given Type[T], ValueOfExpr[T]): ValueOfExpr[Seq[T]] = new {
    def apply(x: Expr[Seq[T]])(given QuoteContext): Option[Seq[T]] = x match {
      case ValueSeq(elems) => Some(elems)
      case '{ scala.collection.Seq[T](${ValueSeq(elems)}: _*) } => Some(elems)
      case '{ scala.collection.immutable.Seq[T](${ValueSeq(elems)}: _*) } => Some(elems)
      case _ => None
    }
    override def toString(): String = "scala.quoted.ValueOfExpr.Seq_delegate"
  }

}
