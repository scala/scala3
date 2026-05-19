package scala.quoted

import language.experimental.captureChecking

/** A type class for types that can convert a `quoted.Expr[T]` to a `T`.
 *
 *  - Converts expression containing literal values to their values:
 *    - `'{1}` -> `1`, `'{2}` -> `2`, ...
 *    - For all primitive types and `String`
 *  - Converts an expression that constructs a copy of its value.
 *    - This expression must be some kind of data structure (`Some`, `List`, `Either`, ...)
 *    - Calls to `new X` or `X.apply` can be lifted into its value
 *    - Arguments of constructors can be recursively unlifted
 *
 *  @tparam T the type of the value that can be extracted from the quoted expression
 */
trait FromExpr[T] {

  /** Returns the value of the expression.
   *
   *  Returns `None` if the expression does not represent a value or possibly contains side effects.
   *  Otherwise returns the `Some` of the value.
   *
   *  @param x the quoted expression to extract a value from
   *  @return `Some(value)` if the expression contains an extractable value, `None` otherwise
   */
  def unapply(x: Expr[T])(using Quotes): Option[T]

}

/** Default given instances of `FromExpr`. */
object FromExpr {

  /** Default implementation of `FromExpr[Boolean]`
   *  - Transform `'{true}` into `Some(true)`
   *  - Transform `'{false}` into `Some(false)`
   *  - Otherwise returns `None`
   */
  given BooleanFromExpr[T <: Boolean]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Byte]`
   *  - Transform `'{n}` into `Some(n)` for a literal `n` of type `Byte`
   *  - Otherwise returns `None`
   */
  given ByteFromExpr[T <: Byte]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Short]`
   *  - Transform `'{n}` into `Some(n)` for a literal `n` of type `Short`
   *  - Otherwise returns `None`
   */
  given ShortFromExpr[T <: Short]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Int]`
   *  - Transform `'{n}` into `Some(n)` for a literal `n` of type `Int`
   *  - Otherwise returns `None`
   */
  given IntFromExpr[T <: Int]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Long]`
   *  - Transform `'{n}` into `Some(n)` for a literal `n` of type `Long`
   *  - Otherwise returns `None`
   */
  given LongFromExpr[T <: Long]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Float]`
   *  - Transform `'{n}` into `Some(n)` for a literal `n` of type `Float`
   *  - Otherwise returns `None`
   */
  given FloatFromExpr[T <: Float]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Double]`
   *  - Transform `'{n}` into `Some(n)` for a literal `n` of type `Double`
   *  - Otherwise returns `None`
   */
  given DoubleFromExpr[T <: Double]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[Char]`
   *  - Transform `'{c}` into `Some(c)` for a literal `c` of type `Char`
   *  - Otherwise returns `None`
   */
  given CharFromExpr[T <: Char]: FromExpr[T] = new PrimitiveFromExpr

  /** Default implementation of `FromExpr[String]`
   *  - Transform `'{str}` into `Some(str)` for a literal `str` of type `String`
   *  - Otherwise returns `None`
   */
  given StringFromExpr[T <: String]: FromExpr[T] = new PrimitiveFromExpr

  /** Lift a quoted primitive value `'{ x }` into `x`.
   *
   *  @tparam T the primitive or `String` type to extract from the quoted literal
   */
  private class PrimitiveFromExpr[T <: Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends FromExpr[T] {
    def unapply(expr: Expr[T])(using Quotes) =
      import quotes.reflect.*
      def rec(tree: Term): Option[T] = tree match {
        case Block(stats, e) => if stats.isEmpty then rec(e) else None
        case Inlined(_, bindings, e) => if bindings.isEmpty then rec(e) else None
        case Typed(e, _) => rec(e)
        case _ =>
          tree.tpe.widenTermRefByName match
            case ConstantType(c) => Some(c.value.asInstanceOf[T])
            case _ => None
      }
      rec(expr.asTerm)
  }

  /** Default implementation of `FromExpr[Option]`
   *  - Transform `'{Some(x)}` into `Some(Some(x))` if `x` can be transformed using `FromExpr[T]`
   *  - Transform `'{None}` into `Some(None)`
   *  - Otherwise returns `None`
   */
  given OptionFromExpr[T](using Type[T], FromExpr[T]): FromExpr[Option[T]] with {
    def unapply(x: Expr[Option[T]])(using Quotes) = x match {
      case '{ Option[T](${Expr(y)}: T) } => Some(Option(y))
      case '{ None } => Some(None)
      case '{ ${Expr(opt)} : Some[T] } => Some(opt)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[None]`
   *  - Transform `'{None}` into `Some(None)`
   *  - Otherwise returns `None`
   */
  given NoneFromExpr: FromExpr[None.type] with {
    def unapply(x: Expr[None.type])(using Quotes) = x match {
      case '{ None } => Some(None)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Some]`
   *  - Transform `'{Some(x)}` into `Some(Some(x))` if `x` can be transformed using `FromExpr[T]`
   *  - Otherwise returns `None`
   */
  given SomeFromExpr[T](using Type[T], FromExpr[T]): FromExpr[Some[T]] with {
    def unapply(x: Expr[Some[T]])(using Quotes) = x match {
      case '{ new Some[T](${Expr(y)}) } => Some(Some(y))
      case '{     Some[T](${Expr(y)}) } => Some(Some(y))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[StringContext]`
   *  - Transform `'{StringContext(args*)}` into `Some(StringContext(args*))` if `args` is explicit and each one is liftable
   *  - Otherwise returns `None`
   */
  given StringContextFromExpr: FromExpr[StringContext] with {
    def unapply(x: Expr[StringContext])(using Quotes) = x match {
      case '{ new StringContext(${Varargs(Exprs(args))}*) } => Some(StringContext(args*))
      case '{     StringContext(${Varargs(Exprs(args))}*) } => Some(StringContext(args*))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[EmptyTuple]`
   *  - Transform `'{EmptyTuple}` into `Some(EmptyTuple)`
   *  - Otherwise returns `None`
   */
  given EmptyTupleFromExpr: FromExpr[EmptyTuple.type] with {
    def unapply(x: Expr[EmptyTuple.type])(using Quotes) = x match {
      case '{ EmptyTuple } => Some(EmptyTuple)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple1[...]]`
   *  - Transform `'{Tuple1(x1)}` into `Some(Tuple1(x1))` if `x1` can be transformed using `FromExpr[T]`
   *  - Otherwise returns `None`
   */
  given Tuple1FromExpr[T1](using Type[T1], FromExpr[T1]): FromExpr[Tuple1[T1]] with {
    def unapply(x: Expr[Tuple1[T1]])(using Quotes) = x match {
      case '{ new Tuple1[T1](${Expr(y)}) } => Some(Tuple1(y))
      case '{     Tuple1[T1](${Expr(y)}) } => Some(Tuple1(y))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple2[...]]`
   *  - Transform `'{Tuple2(x1, x2)}` into `Some(Tuple2(x1, x2))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple2FromExpr[T1, T2](using Type[T1], Type[T2], FromExpr[T1], FromExpr[T2]): FromExpr[Tuple2[T1, T2]] with {
    def unapply(x: Expr[Tuple2[T1, T2]])(using Quotes) = x match {
      case '{ new Tuple2[T1, T2](${Expr(y1)}, ${Expr(y2)}) } => Some(Tuple2(y1, y2))
      case '{     Tuple2[T1, T2](${Expr(y1)}, ${Expr(y2)}) } => Some(Tuple2(y1, y2))
      case '{ (${Expr(y1)}: T1) -> (${Expr(y2)}: T2) } => Some(Tuple2(y1, y2))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple3[...]]`
   *  - Transform `'{Tuple3(x1, x2, x3)}` into `Some(Tuple3(x1, x2, x3))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple3FromExpr[T1, T2, T3](using Type[T1], Type[T2], Type[T3], FromExpr[T1], FromExpr[T2], FromExpr[T3]): FromExpr[Tuple3[T1, T2, T3]] with {
    def unapply(x: Expr[Tuple3[T1, T2, T3]])(using Quotes) = x match {
      case '{ new Tuple3[T1, T2, T3](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}) } => Some(Tuple3(y1, y2, y3))
      case '{     Tuple3[T1, T2, T3](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}) } => Some(Tuple3(y1, y2, y3))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple4[...]]`
   *  - Transform `'{Tuple4(x1, ..., x4)}` into `Some(Tuple4(x1, ..., x4))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple4FromExpr[T1, T2, T3, T4](using Type[T1], Type[T2], Type[T3], Type[T4], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4]): FromExpr[Tuple4[T1, T2, T3, T4]] with {
    def unapply(x: Expr[Tuple4[T1, T2, T3, T4]])(using Quotes) = x match {
      case '{ new Tuple4[T1, T2, T3, T4](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case '{     Tuple4[T1, T2, T3, T4](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple5[...]]`
   *  - Transform `'{Tuple5(x1, ..., x5)}` into `Some(Tuple5(x1, ..., x5))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple5FromExpr[T1, T2, T3, T4, T5](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5]): FromExpr[Tuple5[T1, T2, T3, T4, T5]] with {
    def unapply(x: Expr[Tuple5[T1, T2, T3, T4, T5]])(using Quotes) = x match {
      case '{ new Tuple5[T1, T2, T3, T4, T5](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case '{     Tuple5[T1, T2, T3, T4, T5](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple6[...]]`
   *  - Transform `'{Tuple6(x1, ..., x6)}` into `Some(Tuple6(x1, ..., x6))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple6FromExpr[T1, T2, T3, T4, T5, T6](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6]): FromExpr[Tuple6[T1, T2, T3, T4, T5, T6]] with {
    def unapply(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]])(using Quotes) = x match {
      case '{ new Tuple6[T1, T2, T3, T4, T5, T6](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case '{     Tuple6[T1, T2, T3, T4, T5, T6](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple7[...]]`
   *  - Transform `'{Tuple7(x1, ..., x7)}` into `Some(Tuple7(x1, ..., x7))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple7FromExpr[T1, T2, T3, T4, T5, T6, T7](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7]): FromExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] with {
    def unapply(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]])(using Quotes) = x match {
      case '{ new Tuple7[T1, T2, T3, T4, T5, T6, T7](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case '{     Tuple7[T1, T2, T3, T4, T5, T6, T7](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple8[...]]`
   *  - Transform `'{Tuple8(x1, ..., x8)}` into `Some(Tuple8(x1, ..., x8))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple8FromExpr[T1, T2, T3, T4, T5, T6, T7, T8](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8]): FromExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] with {
    def unapply(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]])(using Quotes) = x match {
      case '{ new Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case '{     Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple9[...]]`
   *  - Transform `'{Tuple9(x1, ..., x9)}` into `Some(Tuple9(x1, ..., x9))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple9FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9]): FromExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] with {
    def unapply(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]])(using Quotes) = x match {
      case '{ new Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case '{     Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple10[...]]`
   *  - Transform `'{Tuple0(x1, ..., x10)}` into `Some(Tuple0(x1, ..., x10))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple10FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10]): FromExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] with {
    def unapply(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]])(using Quotes) = x match {
      case '{ new Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case '{     Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple11[...]]`
   *  - Transform `'{Tuple1(x1, ..., x11)}` into `Some(Tuple1(x1, ..., x11))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple11FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11]): FromExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] with {
    def unapply(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]])(using Quotes) = x match {
      case '{ new Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case '{     Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple12[...]]`
   *  - Transform `'{Tuple2(x1, ..., x12)}` into `Some(Tuple2(x1, ..., x12))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple12FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12]): FromExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] with {
    def unapply(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]])(using Quotes) = x match {
      case '{ new Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case '{     Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple13[...]]`
   *  - Transform `'{Tuple3(x1, ..., x13)}` into `Some(Tuple3(x1, ..., x13))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple13FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13]): FromExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] with {
    def unapply(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]])(using Quotes) = x match {
      case '{ new Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case '{     Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple14[...]]`
   *  - Transform `'{Tuple4(x1, ..., x14)}` into `Some(Tuple4(x1, ..., x14))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple14FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14]): FromExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] with {
    def unapply(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]])(using Quotes) = x match {
      case '{ new Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case '{     Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple15[...]]`
   *  - Transform `'{Tuple5(x1, ..., x15)}` into `Some(Tuple5(x1, ..., x15))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple15FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15]): FromExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] with {
    def unapply(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]])(using Quotes) = x match {
      case '{ new Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case '{     Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple16[...]]`
   *  - Transform `'{Tuple6(x1, ..., x16)}` into `Some(Tuple6(x1, ..., x16))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple16FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16]): FromExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] with {
    def unapply(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]])(using Quotes) = x match {
      case '{ new Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case '{     Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple17[...]]`
   *  - Transform `'{Tuple7(x1, ..., x17)}` into `Some(Tuple7(x1, ..., x17))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple17FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17]): FromExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] with {
    def unapply(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]])(using Quotes) = x match {
      case '{ new Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case '{     Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple18[...]]`
   *  - Transform `'{Tuple8(x1, ..., x18)}` into `Some(Tuple8(x1, ..., x18))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple18FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18]): FromExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] with {
    def unapply(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]])(using Quotes) = x match {
      case '{ new Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case '{     Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple19[...]]`
   *  - Transform `'{Tuple9(x1, ..., x19)}` into `Some(Tuple9(x1, ..., x19))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple19FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19]): FromExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] with {
    def unapply(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]])(using Quotes) = x match {
      case '{ new Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case '{     Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple20[...]]`
   *  - Transform `'{Tuple0(x1, ..., x20)}` into `Some(Tuple0(x1, ..., x20))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple20FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20]): FromExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] with {
    def unapply(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]])(using Quotes) = x match {
      case '{ new Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case '{     Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple21[...]]`
   *  - Transform `'{Tuple1(x1, ..., x21)}` into `Some(Tuple1(x1, ..., x21))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple21FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20], FromExpr[T21]): FromExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] with {
    def unapply(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]])(using Quotes) = x match {
      case '{ new Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case '{     Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Tuple22[...]]`
   *  - Transform `'{Tuple2(x1, ..., x22)}` into `Some(Tuple2(x1, ..., x22))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  given Tuple22FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20], FromExpr[T21], FromExpr[T22]): FromExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] with {
    def unapply(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]])(using Quotes) = x match {
      case '{ new Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}, ${Expr(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case '{     Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}, ${Expr(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Seq]`
   *  - Transform `'{Seq(x1, ..., xn)}` into `Some(Seq(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform sequences that come out of varargs
   *  - Otherwise returns `None`
   */
  given SeqFromExpr[T](using Type[T], FromExpr[T]): FromExpr[Seq[T]] with {
    def unapply(x: Expr[Seq[T]])(using Quotes) = x match {
      case Varargs(Exprs(elems)) => Some(elems)
      case '{ scala.Seq[T](${Varargs(Exprs(elems))}*) } => Some(elems)
      case '{ scala.collection.immutable.Seq[T](${Varargs(Exprs(elems))}*) } => Some(elems)
      case '{  ${Expr(x)}: List[T] } => Some(x)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Nil]`
   *  - Transform `'{Nil}` into `Some(Nil)`
   *  - Otherwise returns `None`
   */
  given NilFromExpr: FromExpr[Nil.type] with {
    def unapply(x: Expr[Nil.type])(using Quotes) = x match {
      case '{ scala.Nil } |  '{ scala.collection.immutable.Nil } => Some(Nil)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[List]`
   *  - Transform `'{List(x1, ..., xn)}` into `Some(List(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{List.empty}` into `Some(Nil)`
   *  - Transform `'{Nil}` into `Some(Nil)`
   *  - Otherwise returns `None`
   */
  given ListFromExpr[T](using Type[T], FromExpr[T]): FromExpr[List[T]] with {
    def unapply(x: Expr[List[T]])(using Quotes) = x match {
      case '{ scala.List[T](${Varargs(Exprs(elems))}*) } => Some(elems.toList)
      case '{ scala.List.empty[T] } => Some(Nil)
      case '{ Nil } => Some(Nil)
      case '{ scala.collection.immutable.List[T](${Varargs(Exprs(elems))}*) } => Some(elems.toList)
      case '{ scala.collection.immutable.List.empty[T] } => Some(Nil)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Set]`
   *  - Transform `'{Set(x1, ..., xn)}` into `Some(Set(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{Set.empty}` into `Some(Set())`
   *  - Otherwise returns `None`
   */
  given SetFromExpr[T](using Type[T], FromExpr[T]): FromExpr[Set[T]] with {
    def unapply(x: Expr[Set[T]])(using Quotes) = x match {
      case '{ Set[T](${Varargs(Exprs(elems))}*) } => Some(elems.toSet)
      case '{ Set.empty[T] } => Some(Set.empty[T])
      case '{ scala.collection.immutable.Set[T](${Varargs(Exprs(elems))}*) } => Some(elems.toSet)
      case '{ scala.collection.immutable.Set.empty[T] } => Some(Set.empty[T])
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Map]`
   *  - Transform `'{Map(x1, ..., xn)}` into `Some(Map(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{Map.empty}` into `Some(Map())`
   *  - Otherwise returns `None`
   */
  given MapFromExpr[T, U](using Type[T], Type[U], FromExpr[T], FromExpr[U]): FromExpr[Map[T, U]] with {
    def unapply(x: Expr[Map[T, U]])(using Quotes) = x match {
      case '{ Map[T, U](${Varargs(Exprs(elems))}*) } => Some(elems.toMap)
      case '{ Map.empty[T, U] } => Some(Map.empty)
      case '{ scala.collection.immutable.Map[T, U](${Varargs(Exprs(elems))}*) } => Some(elems.toMap)
      case '{ scala.collection.immutable.Map.empty[T, U] } => Some(Map.empty)
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Either]`
   *  - Transform `'{Left(x)}` into `Some(Left(x))` if `x` can be transformed using `FromExpr[L]`
   *  - Transform `'{Right(x)}` into `Some(Right(x))` if `x` can be transformed using `FromExpr[R]`
   *  - Otherwise returns `None`
   */
  given EitherFromExpr[L, R](using Type[L], Type[R], FromExpr[L], FromExpr[R]): FromExpr[Either[L, R]] with {
    def unapply(x: Expr[Either[L, R]])(using Quotes) = x match {
      case '{ $x: Left[L, R] } => x.value
      case '{ $x: Right[L, R] } => x.value
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Left]`
   *  - Transform `'{Left(x)}` into `Some(Left(x))` if `x` can be transformed using `FromExpr[L]`
   *  - Otherwise returns `None`
   */
  given LeftFromExpr[L, R](using Type[L], Type[R], FromExpr[L]): FromExpr[Left[L, R]] with {
    def unapply(x: Expr[Left[L, R]])(using Quotes) = x match {
      case '{ Left[L, R](${Expr(x)}) } => Some(Left(x))
      case _ => None
    }
  }

  /** Default implementation of `FromExpr[Right]`
   *  - Transform `'{Right(x)}` into `Some(Right(x))` if `x` can be transformed using `FromExpr[R]`
   *  - Otherwise returns `None`
   */
  given RightFromExpr[L, R](using Type[L], Type[R], FromExpr[R]): FromExpr[Right[L, R]] with {
    def unapply(x: Expr[Right[L, R]])(using Quotes) = x match {
      case '{ Right[L, R](${Expr(x)}) } => Some(Right(x))
      case _ => None
    }
  }

}
