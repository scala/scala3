package scala.quoted

/** A type class for types that can convert a `quoted.Expr[T]` to a `T`.
 *
 *  - Converts expression containg literal values to their values:
 *    - `'{1}` -> `1`, `'{2}` -> `2`, ...
 *    - For all primitive types and `String`
 *  - Converts an expression that constructs a value into its value.
 *    - This expression must be some kind of datastructure (`Some`, `List`, `Either`, ...)
 *    - Calls to `new X` or `X.apply` can be lifted into its value
 *    - Arguments of constructors can be recursively unlifted
 */
trait Unliftable[T] {

  /** Return the value of the expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  def fromExpr(x: Expr[T]): Quotes ?=> Option[T]

}

object Unliftable {

  /** Default unliftable for Boolean
   *  - Unlifts `'{true}` into `Some(ture)`
   *  - Unlifts `'{false}` into `Some(false)`
   *  - Otherwise unlifts to `None`
   */
  given BooleanUnliftable[T <: Boolean] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Byte
   *  - Unlifts `'{n}` into `Some(n)` for a literal `n` of type `Byte`
   *  - Otherwise unlifts to `None`
   */
  given ByteUnliftable[T <: Byte] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Short
   *  - Unlifts `'{n}` into `Some(n)` for a literal `n` of type `Short`
   *  - Otherwise unlifts to `None`
   */
  given ShortUnliftable[T <: Short] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Int
   *  - Unlifts `'{n}` into `Some(n)` for a literal `n` of type `Int`
   *  - Otherwise unlifts to `None`
   */
  given IntUnliftable[T <: Int] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Long
   *  - Unlifts `'{n}` into `Some(n)` for a literal `n` of type `Long`
   *  - Otherwise unlifts to `None`
   */
  given LongUnliftable[T <: Long] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Float
   *  - Unlifts `'{n}` into `Some(n)` for a literal `n` of type `Float`
   *  - Otherwise unlifts to `None`
   */
  given FloatUnliftable[T <: Float] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Double
   *  - Unlifts `'{n}` into `Some(n)` for a literal `n` of type `Double`
   *  - Otherwise unlifts to `None`
   */
  given DoubleUnliftable[T <: Double] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for Char
   *  - Unlifts `'{c}` into `Some(c)` for a literal `c` of type `Char`
   *  - Otherwise unlifts to `None`
   */
  given CharUnliftable[T <: Char] as Unliftable[T] = new PrimitiveUnliftable

  /** Default unliftable for String
   *  - Unlifts `'{str}` into `Some(str)` for a literal `str` of type `String`
   *  - Otherwise unlifts to `None`
   */
  given StringUnliftable[T <: String] as Unliftable[T] = new PrimitiveUnliftable

  /** Lift a quoted primitive value `'{ x }` into `x` */
  private class PrimitiveUnliftable[T <: Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends Unliftable[T] {
    def fromExpr(expr: Expr[T]) =
      import quotes.reflect._
      def rec(tree: Term): Option[T] = tree match {
        case Literal(c) if c.value != null => Some(c.value.asInstanceOf[T])
        case Block(Nil, e) => rec(e)
        case Typed(e, _) => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _  => None
      }
      rec(Term.of(expr))
  }

  /** Default unliftable for Option
   *  - Unlifts `'{Some(x)}` into `Some(Some(x))` if `x` is unliftable
   *  - Unlifts `'{None}` into `Some(None)`
   *  - Otherwise unlifts to `None`
   */
  given OptionUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Option[T]] = new {
    def fromExpr(x: Expr[Option[T]]) = x match {
      case '{ Option[T](${Expr(y)}) } => Some(Option(y))
      case '{ None } => Some(None)
      case '{ ${Expr(opt)} : Some[T] } => Some(opt)
      case _ => None
    }
  }

  /** Default unliftable for None
   *  - Unlifts `'{None}` into `Some(None)`
   *  - Otherwise unlifts to `None`
   */
  given NoneUnliftable as Unliftable[None.type] = new {
    def fromExpr(x: Expr[None.type]) = x match {
      case '{ None } => Some(None)
      case _ => None
    }
  }

  /** Default unliftable for Some
   *  - Unlifts `'{Some(x)}` into `Some(Some(x))` if `x` is unliftable
   *  - Otherwise unlifts to `None`
   */
  given SomeUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Some[T]] = new {
    def fromExpr(x: Expr[Some[T]]) = x match {
      case '{ new Some[T](${Expr(y)}) } => Some(Some(y))
      case '{     Some[T](${Expr(y)}) } => Some(Some(y))
      case _ => None
    }
  }

  /** Default unliftable for StringContext
   *  - Unlifts `'{StringContext(args: _*)}` into `Some(StringContext(args: _*))` if `args` is explicit and each one is liftable
   *  - Otherwise unlifts to `None`
   */
  given StringContextUnliftable as Unliftable[StringContext] = new {
    def fromExpr(x: Expr[StringContext]) = x match {
      case '{ new StringContext(${Varargs(Consts(args))}: _*) } => Some(StringContext(args: _*))
      case '{     StringContext(${Varargs(Consts(args))}: _*) } => Some(StringContext(args: _*))
      case _ => None
    }
  }

  /** Default unliftable for EmptyTuple
   *  - Unlifts `'{EmptyTuple}` into `Some(EmptyTuple)`
   *  - Otherwise unlifts to `None`
   */
  given EmptyTupleUnliftable as Unliftable[EmptyTuple.type] = new {
    def fromExpr(x: Expr[EmptyTuple.type]) = x match {
      case '{ EmptyTuple } => Some(EmptyTuple)
      case _ => None
    }
  }

  /** Default unliftable for Tuple1
   *  - Unlifts `'{Tuple1(x1)}` into `Some(Tuple1(x1))` if `x1` is unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple1Unliftable[T1](using Type[T1], Unliftable[T1]) as Unliftable[Tuple1[T1]] = new {
    def fromExpr(x: Expr[Tuple1[T1]]) = x match {
      case '{ new Tuple1[T1](${Expr(y)}) } => Some(Tuple1(y))
      case '{     Tuple1[T1](${Expr(y)}) } => Some(Tuple1(y))
      case _ => None
    }
  }

  /** Default unliftable for Tuple2
   *  - Unlifts `'{Tuple2(x1, x2)}` into `Some(Tuple2(x1, x2))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple2Unliftable[T1, T2](using Type[T1], Type[T2], Unliftable[T1], Unliftable[T2]) as Unliftable[Tuple2[T1, T2]] = new {
    def fromExpr(x: Expr[Tuple2[T1, T2]]) = x match {
      case '{ new Tuple2[T1, T2](${Expr(y1)}, ${Expr(y2)}) } => Some(Tuple2(y1, y2))
      case '{     Tuple2[T1, T2](${Expr(y1)}, ${Expr(y2)}) } => Some(Tuple2(y1, y2))
      case '{ (${Expr(y1)}: T1) -> (${Expr(y2)}: T2) } => Some(Tuple2(y1, y2))
      case _ => None
    }
  }

  /** Default unliftable for Tuple3
   *  - Unlifts `'{Tuple3(x1, x2, x3)}` into `Some(Tuple3(x1, x2, x3))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple3Unliftable[T1, T2, T3](using Type[T1], Type[T2], Type[T3], Unliftable[T1], Unliftable[T2], Unliftable[T3]) as Unliftable[Tuple3[T1, T2, T3]] = new {
    def fromExpr(x: Expr[Tuple3[T1, T2, T3]]) = x match {
      case '{ new Tuple3[T1, T2, T3](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}) } => Some(Tuple3(y1, y2, y3))
      case '{     Tuple3[T1, T2, T3](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}) } => Some(Tuple3(y1, y2, y3))
      case _ => None
    }
  }

  /** Default unliftable for Tuple4
   *  - Unlifts `'{Tuple4(x1, ..., x4)}` into `Some(Tuple4(x1, ..., x4))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple4Unliftable[T1, T2, T3, T4](using Type[T1], Type[T2], Type[T3], Type[T4], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4]) as Unliftable[Tuple4[T1, T2, T3, T4]] = new {
    def fromExpr(x: Expr[Tuple4[T1, T2, T3, T4]]) = x match {
      case '{ new Tuple4[T1, T2, T3, T4](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case '{     Tuple4[T1, T2, T3, T4](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case _ => None
    }
  }

  /** Default unliftable for Tuple5
   *  - Unlifts `'{Tuple5(x1, ..., x5)}` into `Some(Tuple5(x1, ..., x5))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple5Unliftable[T1, T2, T3, T4, T5](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5]) as Unliftable[Tuple5[T1, T2, T3, T4, T5]] = new {
    def fromExpr(x: Expr[Tuple5[T1, T2, T3, T4, T5]]) = x match {
      case '{ new Tuple5[T1, T2, T3, T4, T5](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case '{     Tuple5[T1, T2, T3, T4, T5](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case _ => None
    }
  }

  /** Default unliftable for Tuple6
   *  - Unlifts `'{Tuple6(x1, ..., x6)}` into `Some(Tuple6(x1, ..., x6))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple6Unliftable[T1, T2, T3, T4, T5, T6](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6]) as Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def fromExpr(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]]) = x match {
      case '{ new Tuple6[T1, T2, T3, T4, T5, T6](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case '{     Tuple6[T1, T2, T3, T4, T5, T6](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case _ => None
    }
  }

  /** Default unliftable for Tuple7
   *  - Unlifts `'{Tuple7(x1, ..., x7)}` into `Some(Tuple7(x1, ..., x7))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple7Unliftable[T1, T2, T3, T4, T5, T6, T7](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7]) as Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def fromExpr(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]) = x match {
      case '{ new Tuple7[T1, T2, T3, T4, T5, T6, T7](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case '{     Tuple7[T1, T2, T3, T4, T5, T6, T7](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case _ => None
    }
  }

  /** Default unliftable for Tuple8
   *  - Unlifts `'{Tuple8(x1, ..., x8)}` into `Some(Tuple8(x1, ..., x8))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple8Unliftable[T1, T2, T3, T4, T5, T6, T7, T8](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8]) as Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def fromExpr(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]) = x match {
      case '{ new Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case '{     Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case _ => None
    }
  }

  /** Default unliftable for Tuple9
   *  - Unlifts `'{Tuple9(x1, ..., x9)}` into `Some(Tuple9(x1, ..., x9))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple9Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9]) as Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def fromExpr(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]) = x match {
      case '{ new Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case '{     Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case _ => None
    }
  }

  /** Default unliftable for Tuple10
   *  - Unlifts `'{Tuple0(x1, ..., x10)}` into `Some(Tuple0(x1, ..., x10))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple10Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10]) as Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def fromExpr(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]) = x match {
      case '{ new Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case '{     Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case _ => None
    }
  }

  /** Default unliftable for Tuple11
   *  - Unlifts `'{Tuple1(x1, ..., x11)}` into `Some(Tuple1(x1, ..., x11))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple11Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11]) as Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def fromExpr(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]) = x match {
      case '{ new Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case '{     Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case _ => None
    }
  }

  /** Default unliftable for Tuple12
   *  - Unlifts `'{Tuple2(x1, ..., x12)}` into `Some(Tuple2(x1, ..., x12))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple12Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12]) as Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def fromExpr(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]) = x match {
      case '{ new Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case '{     Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case _ => None
    }
  }

  /** Default unliftable for Tuple13
   *  - Unlifts `'{Tuple3(x1, ..., x13)}` into `Some(Tuple3(x1, ..., x13))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple13Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13]) as Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def fromExpr(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]) = x match {
      case '{ new Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case '{     Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case _ => None
    }
  }

  /** Default unliftable for Tuple14
   *  - Unlifts `'{Tuple4(x1, ..., x14)}` into `Some(Tuple4(x1, ..., x14))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple14Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14]) as Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def fromExpr(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]) = x match {
      case '{ new Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case '{     Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case _ => None
    }
  }

  /** Default unliftable for Tuple15
   *  - Unlifts `'{Tuple5(x1, ..., x15)}` into `Some(Tuple5(x1, ..., x15))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple15Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15]) as Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def fromExpr(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]) = x match {
      case '{ new Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case '{     Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case _ => None
    }
  }

  /** Default unliftable for Tuple16
   *  - Unlifts `'{Tuple6(x1, ..., x16)}` into `Some(Tuple6(x1, ..., x16))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple16Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16]) as Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def fromExpr(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]) = x match {
      case '{ new Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case '{     Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case _ => None
    }
  }

  /** Default unliftable for Tuple17
   *  - Unlifts `'{Tuple7(x1, ..., x17)}` into `Some(Tuple7(x1, ..., x17))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple17Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17]) as Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def fromExpr(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]) = x match {
      case '{ new Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case '{     Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case _ => None
    }
  }

  /** Default unliftable for Tuple18
   *  - Unlifts `'{Tuple8(x1, ..., x18)}` into `Some(Tuple8(x1, ..., x18))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple18Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18]) as Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def fromExpr(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]) = x match {
      case '{ new Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case '{     Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case _ => None
    }
  }

  /** Default unliftable for Tuple19
   *  - Unlifts `'{Tuple9(x1, ..., x19)}` into `Some(Tuple9(x1, ..., x19))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple19Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19]) as Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def fromExpr(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) = x match {
      case '{ new Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case '{     Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case _ => None
    }
  }

  /** Default unliftable for Tuple20
   *  - Unlifts `'{Tuple0(x1, ..., x20)}` into `Some(Tuple0(x1, ..., x20))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple20Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20]) as Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def fromExpr(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]) = x match {
      case '{ new Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case '{     Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case _ => None
    }
  }

  /** Default unliftable for Tuple21
   *  - Unlifts `'{Tuple1(x1, ..., x21)}` into `Some(Tuple1(x1, ..., x21))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple21Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20], Unliftable[T21]) as Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def fromExpr(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]) = x match {
      case '{ new Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case '{     Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case _ => None
    }
  }

  /** Default unliftable for Tuple22
   *  - Unlifts `'{Tuple2(x1, ..., x22)}` into `Some(Tuple2(x1, ..., x22))` if all `xi` are unliftable
   *  - Otherwise unlifts to `None`
   */
  given Tuple22Unliftable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], Unliftable[T1], Unliftable[T2], Unliftable[T3], Unliftable[T4], Unliftable[T5], Unliftable[T6], Unliftable[T7], Unliftable[T8], Unliftable[T9], Unliftable[T10], Unliftable[T11], Unliftable[T12], Unliftable[T13], Unliftable[T14], Unliftable[T15], Unliftable[T16], Unliftable[T17], Unliftable[T18], Unliftable[T19], Unliftable[T20], Unliftable[T21], Unliftable[T22]) as Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def fromExpr(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]) = x match {
      case '{ new Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}, ${Expr(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case '{     Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}, ${Expr(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case _ => None
    }
  }

  /** Default unliftable for Seq
   *  - Unlifts `'{Seq(x1, ..., xn)}` into `Some(Seq(x1, ..., xn))` if all `xi` are unliftable
   *  - Unlifts sequences that come out of varargs
   *  - Otherwise unlifts to `None`
   */
  given SeqUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Seq[T]] = new {
    def fromExpr(x: Expr[Seq[T]]) = x match {
      case Varargs(Exprs(elems)) => Some(elems)
      case '{ scala.Seq[T](${Varargs(Exprs(elems))}: _*) } => Some(elems)
      case '{ scala.collection.immutable.Seq[T](${Varargs(Exprs(elems))}: _*) } => Some(elems)
      case '{  ${Expr(x)}: List[T] } => Some(x)
      case _ => None
    }
  }

  /** Default unliftable for Nil
   *  - Unlifts `'{Nil}` into `Some(Nil)`
   *  - Otherwise unlifts to `None`
   */
  given NilUnliftable as Unliftable[Nil.type] = new {
    def fromExpr(x: Expr[Nil.type]) = x match {
      case '{ scala.Nil } |  '{ scala.collection.immutable.Nil } => Some(Nil)
      case _ => None
    }
  }

  /** Default unliftable for List
   *  - Unlifts `'{List(x1, ..., xn)}` into `Some(List(x1, ..., xn))` if all `xi` are unliftable
   *  - Unlifts `'{List.empty}` into `Some(Nil)`
   *  - Unlifts `'{Nil}` into `Some(Nil)`
   *  - Otherwise unlifts to `None`
   */
  given ListUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[List[T]] = new {
    def fromExpr(x: Expr[List[T]]) = x match {
      case '{ scala.List[T](${Varargs(Exprs(elems))}: _*) } => Some(elems.toList)
      case '{ scala.List.empty[T] } => Some(Nil)
      case '{ Nil } => Some(Nil)
      case '{ scala.collection.immutable.List[T](${Varargs(Exprs(elems))}: _*) } => Some(elems.toList)
      case '{ scala.collection.immutable.List.empty[T] } => Some(Nil)
      case _ => None
    }
  }

  /** Default unliftable for Set
   *  - Unlifts `'{Set(x1, ..., xn)}` into `Some(Set(x1, ..., xn))` if all `xi` are unliftable
   *  - Unlifts `'{Set.empty}` into `Some(Set())`
   *  - Otherwise unlifts to `None`
   */
  given SetUnliftable[T](using Type[T], Unliftable[T]) as Unliftable[Set[T]] = new {
    def fromExpr(x: Expr[Set[T]]) = x match {
      case '{ Set[T](${Varargs(Exprs(elems))}: _*) } => Some(elems.toSet)
      case '{ Set.empty[T] } => Some(Set.empty[T])
      case '{ scala.collection.immutable.Set[T](${Varargs(Exprs(elems))}: _*) } => Some(elems.toSet)
      case '{ scala.collection.immutable.Set.empty[T] } => Some(Set.empty[T])
      case _ => None
    }
  }

  /** Default unliftable for Map
   *  - Unlifts `'{Map(x1, ..., xn)}` into `Some(Map(x1, ..., xn))` if all `xi` are unliftable
   *  - Unlifts `'{Map.empty}` into `Some(Map())`
   *  - Otherwise unlifts to `None`
   */
  given MapUnliftable[T, U](using Type[T], Type[U], Unliftable[T], Unliftable[U]) as Unliftable[Map[T, U]] = new {
    def fromExpr(x: Expr[Map[T, U]]) = x match {
      case '{ Map[T, U](${Varargs(Exprs(elems))}: _*) } => Some(elems.toMap)
      case '{ Map.empty[T, U] } => Some(Map.empty)
      case '{ scala.collection.immutable.Map[T, U](${Varargs(Exprs(elems))}: _*) } => Some(elems.toMap)
      case '{ scala.collection.immutable.Map.empty[T, U] } => Some(Map.empty)
      case _ => None
    }
  }

  /** Default unliftable for Either
   *  - Unlifts `'{Left(x)}` into `Some(Left(x))` if `x` is unliftable
   *  - Unlifts `'{Right(x)}` into `Some(Right(x))` if `x` is unliftable
   *  - Otherwise unlifts to `None`
   */
  given EitherUnliftable[L, R](using Type[L], Type[R], Unliftable[L], Unliftable[R]) as Unliftable[Either[L, R]] = new {
    def fromExpr(x: Expr[Either[L, R]]) = x match {
      case '{ $x: Left[L, R] } => x.unlift
      case '{ $x: Right[L, R] } => x.unlift
      case _ => None
    }
  }

  /** Default unliftable for Left
   *  - Unlifts `'{Left(x)}` into `Some(Left(x))` if `x` is unliftable
   *  - Otherwise unlifts to `None`
   */
  given LeftUnliftable[L, R](using Type[L], Type[R], Unliftable[L]) as Unliftable[Left[L, R]] = new {
    def fromExpr(x: Expr[Left[L, R]]) = x match {
      case '{ Left[L, R](${Expr(x)}) } => Some(Left(x))
      case _ => None
    }
  }

  /** Default unliftable for Right
   *  - Unlifts `'{Right(x)}` into `Some(Right(x))` if `x` is unliftable
   *  - Otherwise unlifts to `None`
   */
  given RightUnliftable[L, R](using Type[L], Type[R], Unliftable[R]) as Unliftable[Right[L, R]] = new {
    def fromExpr(x: Expr[Right[L, R]]) = x match {
      case '{ Right[L, R](${Expr(x)}) } => Some(Right(x))
      case _ => None
    }
  }

}
