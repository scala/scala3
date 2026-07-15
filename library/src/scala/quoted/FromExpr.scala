package scala.quoted

import language.experimental.captureChecking
import scala.annotation.{nowarn, tailrec}

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


private[quoted] trait LowPriorityFromExpr:
  given fromFactory: [T: Type] => (f: FromExprFactory[T]) => FromExpr[T] = f.apply()

/** Default given instances of `FromExpr`. */
object FromExpr extends LowPriorityFromExpr:

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
  given OptionFromExpr: [T: {Type, FromExpr}] => (factory: FromExprFactory[Option[T]]) => FromExpr[Option[T]]:
    def unapply(x: Expr[Option[T]])(using Quotes): Option[Option[T]] = factory.apply().unapply(x)

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
  given SomeFromExpr: [T: {Type, FromExpr}] => (factory: FromExprFactory[Some[T]]) => FromExpr[Some[T]]:
    def unapply(x: Expr[Some[T]])(using Quotes): Option[Some[T]] = factory.apply().unapply(x)

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

  /** Default implementation of `FromExpr[Tuple1[T1]]`. Proxies to `FromExprFactory.tuple1FromExprFactory`. */
  given Tuple1FromExpr: [T1: {Type, FromExpr}] => (factory: FromExprFactory[Tuple1[T1]]) => FromExpr[Tuple1[T1]]:
    def unapply(x: Expr[Tuple1[T1]])(using Quotes): Option[Tuple1[T1]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple2[T1, T2]]`. Proxies to `FromExprFactory.tuple2FromExprFactory`. */
  given Tuple2FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}] => (factory: FromExprFactory[Tuple2[T1, T2]]) => FromExpr[Tuple2[T1, T2]]:
    def unapply(x: Expr[Tuple2[T1, T2]])(using Quotes): Option[Tuple2[T1, T2]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple3[T1, T2, T3]]`. Proxies to `FromExprFactory.tuple3FromExprFactory`. */
  given Tuple3FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}] => (factory: FromExprFactory[Tuple3[T1, T2, T3]]) => FromExpr[Tuple3[T1, T2, T3]]:
    def unapply(x: Expr[Tuple3[T1, T2, T3]])(using Quotes): Option[Tuple3[T1, T2, T3]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple4[T1, T2, T3, T4]]`. Proxies to `FromExprFactory.tuple4FromExprFactory`. */
  given Tuple4FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}] => (factory: FromExprFactory[Tuple4[T1, T2, T3, T4]]) => FromExpr[Tuple4[T1, T2, T3, T4]]:
    def unapply(x: Expr[Tuple4[T1, T2, T3, T4]])(using Quotes): Option[Tuple4[T1, T2, T3, T4]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple5[T1, T2, T3, T4, T5]]`. Proxies to `FromExprFactory.tuple5FromExprFactory`. */
  given Tuple5FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}] => (factory: FromExprFactory[Tuple5[T1, T2, T3, T4, T5]]) => FromExpr[Tuple5[T1, T2, T3, T4, T5]]:
    def unapply(x: Expr[Tuple5[T1, T2, T3, T4, T5]])(using Quotes): Option[Tuple5[T1, T2, T3, T4, T5]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple6[T1, T2, T3, T4, T5, T6]]`. Proxies to `FromExprFactory.tuple6FromExprFactory`. */
  given Tuple6FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}] => (factory: FromExprFactory[Tuple6[T1, T2, T3, T4, T5, T6]]) => FromExpr[Tuple6[T1, T2, T3, T4, T5, T6]]:
    def unapply(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]])(using Quotes): Option[Tuple6[T1, T2, T3, T4, T5, T6]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]`. Proxies to `FromExprFactory.tuple7FromExprFactory`. */
  given Tuple7FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}] => (factory: FromExprFactory[Tuple7[T1, T2, T3, T4, T5, T6, T7]]) => FromExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
    def unapply(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]])(using Quotes): Option[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]`. Proxies to `FromExprFactory.tuple8FromExprFactory`. */
  given Tuple8FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}] => (factory: FromExprFactory[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]) => FromExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
    def unapply(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]])(using Quotes): Option[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]`. Proxies to `FromExprFactory.tuple9FromExprFactory`. */
  given Tuple9FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}] => (factory: FromExprFactory[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]) => FromExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
    def unapply(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]])(using Quotes): Option[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]`. Proxies to `FromExprFactory.tuple10FromExprFactory`. */
  given Tuple10FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}] => (factory: FromExprFactory[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]) => FromExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
    def unapply(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]])(using Quotes): Option[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]`. Proxies to `FromExprFactory.tuple11FromExprFactory`. */
  given Tuple11FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}] => (factory: FromExprFactory[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]) => FromExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
    def unapply(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]])(using Quotes): Option[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]`. Proxies to `FromExprFactory.tuple12FromExprFactory`. */
  given Tuple12FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}] => (factory: FromExprFactory[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]) => FromExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
    def unapply(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]])(using Quotes): Option[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]`. Proxies to `FromExprFactory.tuple13FromExprFactory`. */
  given Tuple13FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}] => (factory: FromExprFactory[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]) => FromExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
    def unapply(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]])(using Quotes): Option[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]`. Proxies to `FromExprFactory.tuple14FromExprFactory`. */
  given Tuple14FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}] => (factory: FromExprFactory[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]) => FromExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
    def unapply(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]])(using Quotes): Option[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]`. Proxies to `FromExprFactory.tuple15FromExprFactory`. */
  given Tuple15FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}] => (factory: FromExprFactory[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]) => FromExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
    def unapply(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]])(using Quotes): Option[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]`. Proxies to `FromExprFactory.tuple16FromExprFactory`. */
  given Tuple16FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}] => (factory: FromExprFactory[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]) => FromExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
    def unapply(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]])(using Quotes): Option[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]`. Proxies to `FromExprFactory.tuple17FromExprFactory`. */
  given Tuple17FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}, T17: {Type, FromExpr}] => (factory: FromExprFactory[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]) => FromExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
    def unapply(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]])(using Quotes): Option[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]`. Proxies to `FromExprFactory.tuple18FromExprFactory`. */
  given Tuple18FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}, T17: {Type, FromExpr}, T18: {Type, FromExpr}] => (factory: FromExprFactory[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]) => FromExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
    def unapply(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]])(using Quotes): Option[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]`. Proxies to `FromExprFactory.tuple19FromExprFactory`. */
  given Tuple19FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}, T17: {Type, FromExpr}, T18: {Type, FromExpr}, T19: {Type, FromExpr}] => (factory: FromExprFactory[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) => FromExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
    def unapply(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]])(using Quotes): Option[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]`. Proxies to `FromExprFactory.tuple20FromExprFactory`. */
  given Tuple20FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}, T17: {Type, FromExpr}, T18: {Type, FromExpr}, T19: {Type, FromExpr}, T20: {Type, FromExpr}] => (factory: FromExprFactory[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]) => FromExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
    def unapply(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]])(using Quotes): Option[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]`. Proxies to `FromExprFactory.tuple21FromExprFactory`. */
  given Tuple21FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}, T17: {Type, FromExpr}, T18: {Type, FromExpr}, T19: {Type, FromExpr}, T20: {Type, FromExpr}, T21: {Type, FromExpr}] => (factory: FromExprFactory[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]) => FromExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
    def unapply(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]])(using Quotes): Option[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]`. Proxies to `FromExprFactory.tuple22FromExprFactory`. */
  given Tuple22FromExpr: [T1: {Type, FromExpr}, T2: {Type, FromExpr}, T3: {Type, FromExpr}, T4: {Type, FromExpr}, T5: {Type, FromExpr}, T6: {Type, FromExpr}, T7: {Type, FromExpr}, T8: {Type, FromExpr}, T9: {Type, FromExpr}, T10: {Type, FromExpr}, T11: {Type, FromExpr}, T12: {Type, FromExpr}, T13: {Type, FromExpr}, T14: {Type, FromExpr}, T15: {Type, FromExpr}, T16: {Type, FromExpr}, T17: {Type, FromExpr}, T18: {Type, FromExpr}, T19: {Type, FromExpr}, T20: {Type, FromExpr}, T21: {Type, FromExpr}, T22: {Type, FromExpr}] => (factory: FromExprFactory[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]) => FromExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
    def unapply(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]])(using Quotes): Option[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Seq]`
   *  - Transform `'{Seq(x1, ..., xn)}` into `Some(Seq(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform sequences that come out of varargs
   *  - Otherwise returns `None`
   */
  given SeqFromExpr: [T: {Type, FromExpr}] => (factory: FromExprFactory[Seq[T]]) => FromExpr[Seq[T]]:
    def unapply(x: Expr[Seq[T]])(using Quotes): Option[Seq[T]] = factory.apply().unapply(x)

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
  given ListFromExpr: [T: {Type, FromExpr}] => (factory: FromExprFactory[List[T]]) => FromExpr[List[T]]:
    def unapply(x: Expr[List[T]])(using Quotes): Option[List[T]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Set]`
   *  - Transform `'{Set(x1, ..., xn)}` into `Some(Set(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{Set.empty}` into `Some(Set())`
   *  - Otherwise returns `None`
   */
  given SetFromExpr: [T: {Type, FromExpr}] => (factory: FromExprFactory[Set[T]]) => FromExpr[Set[T]]:
    def unapply(x: Expr[Set[T]])(using Quotes): Option[Set[T]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Map]`
   *  - Transform `'{Map(x1, ..., xn)}` into `Some(Map(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{Map.empty}` into `Some(Map())`
   *  - Otherwise returns `None`
   */
  given MapFromExpr: [T: {Type, FromExpr}, U: {Type, FromExpr}] => (factory: FromExprFactory[Map[T, U]]) => FromExpr[Map[T, U]]:
    def unapply(x: Expr[Map[T, U]])(using Quotes): Option[Map[T, U]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Either]`
   *  - Transform `'{Left(x)}` into `Some(Left(x))` if `x` can be transformed using `FromExpr[L]`
   *  - Transform `'{Right(x)}` into `Some(Right(x))` if `x` can be transformed using `FromExpr[R]`
   *  - Otherwise returns `None`
   */
  given EitherFromExpr: [L: {Type, FromExpr}, R: {Type, FromExpr}] => (factory: FromExprFactory[Either[L, R]]) => FromExpr[Either[L, R]]:
    def unapply(x: Expr[Either[L, R]])(using Quotes): Option[Either[L, R]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Left]`
   *  - Transform `'{Left(x)}` into `Some(Left(x))` if `x` can be transformed using `FromExpr[L]`
   *  - Otherwise returns `None`
   */
  given LeftFromExpr: [L: {Type, FromExpr}, R: Type] => (factory: FromExprFactory[Left[L, R]]) => FromExpr[Left[L, R]]:
    def unapply(x: Expr[Left[L, R]])(using Quotes): Option[Left[L, R]] = factory.apply().unapply(x)

  /** Default implementation of `FromExpr[Right]`
   *  - Transform `'{Right(x)}` into `Some(Right(x))` if `x` can be transformed using `FromExpr[R]`
   *  - Otherwise returns `None`
   */
  given RightFromExpr: [L: Type, R: {Type, FromExpr}] => (factory: FromExprFactory[Right[L, R]]) => FromExpr[Right[L, R]]:
    def unapply(x: Expr[Right[L, R]])(using Quotes): Option[Right[L, R]] = factory.apply().unapply(x)
