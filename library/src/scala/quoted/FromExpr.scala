package scala.quoted

import language.experimental.captureChecking
import scala.annotation.publicInBinary

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
object FromExpr extends LowPriorityFromExpr {

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
  class OptionFromExpr[T] @publicInBinary private[quoted] (using Type[T], FromExpr[T]) extends FromExpr[Option[T]]:
    def unapply(x: Expr[Option[T]])(using Quotes): Option[Option[T]] =
      FromExprFactory.optionFromExprFactory[T].apply().unapply(x)

  @publicInBinary private[quoted] final def OptionFromExpr[T](using Type[T], FromExpr[T]): OptionFromExpr[T] = new OptionFromExpr[T]

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
  class SomeFromExpr[T] @publicInBinary private[quoted] (using Type[T], FromExpr[T]) extends FromExpr[Some[T]]:
    def unapply(x: Expr[Some[T]])(using Quotes): Option[Some[T]] =
      FromExprFactory.someFromExprFactory[T].apply().unapply(x)

  @publicInBinary private[quoted] final def SomeFromExpr[T](using Type[T], FromExpr[T]): SomeFromExpr[T] = new SomeFromExpr[T]

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
  class Tuple1FromExpr[T1] @publicInBinary private[quoted] (using Type[T1], FromExpr[T1]) extends FromExpr[Tuple1[T1]]:
    def unapply(x: Expr[Tuple1[T1]])(using Quotes): Option[Tuple1[T1]] =
      FromExprFactory.tuple1FromExprFactory[T1].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple1FromExpr[T1](using Type[T1], FromExpr[T1]): Tuple1FromExpr[T1] = new Tuple1FromExpr[T1]

  /** Default implementation of `FromExpr[Tuple2[...]]`
   *  - Transform `'{Tuple2(x1, x2)}` into `Some(Tuple2(x1, x2))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple2FromExpr[T1, T2] @publicInBinary private[quoted] (using Type[T1], Type[T2], FromExpr[T1], FromExpr[T2]) extends FromExpr[Tuple2[T1, T2]]:
    def unapply(x: Expr[Tuple2[T1, T2]])(using Quotes): Option[Tuple2[T1, T2]] =
      FromExprFactory.tuple2FromExprFactory[T1, T2].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple2FromExpr[T1, T2](using Type[T1], Type[T2], FromExpr[T1], FromExpr[T2]): Tuple2FromExpr[T1, T2] = new Tuple2FromExpr[T1, T2]

  /** Default implementation of `FromExpr[Tuple3[...]]`
   *  - Transform `'{Tuple3(x1, x2, x3)}` into `Some(Tuple3(x1, x2, x3))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple3FromExpr[T1, T2, T3] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], FromExpr[T1], FromExpr[T2], FromExpr[T3]) extends FromExpr[Tuple3[T1, T2, T3]]:
    def unapply(x: Expr[Tuple3[T1, T2, T3]])(using Quotes): Option[Tuple3[T1, T2, T3]] =
      FromExprFactory.tuple3FromExprFactory[T1, T2, T3].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple3FromExpr[T1, T2, T3](using Type[T1], Type[T2], Type[T3], FromExpr[T1], FromExpr[T2], FromExpr[T3]): Tuple3FromExpr[T1, T2, T3] = new Tuple3FromExpr[T1, T2, T3]

  /** Default implementation of `FromExpr[Tuple4[...]]`
   *  - Transform `'{Tuple4(x1, ..., x4)}` into `Some(Tuple4(x1, ..., x4))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple4FromExpr[T1, T2, T3, T4] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4]) extends FromExpr[Tuple4[T1, T2, T3, T4]]:
    def unapply(x: Expr[Tuple4[T1, T2, T3, T4]])(using Quotes): Option[Tuple4[T1, T2, T3, T4]] =
      FromExprFactory.tuple4FromExprFactory[T1, T2, T3, T4].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple4FromExpr[T1, T2, T3, T4](using Type[T1], Type[T2], Type[T3], Type[T4], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4]): Tuple4FromExpr[T1, T2, T3, T4] = new Tuple4FromExpr[T1, T2, T3, T4]

  /** Default implementation of `FromExpr[Tuple5[...]]`
   *  - Transform `'{Tuple5(x1, ..., x5)}` into `Some(Tuple5(x1, ..., x5))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple5FromExpr[T1, T2, T3, T4, T5] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5]) extends FromExpr[Tuple5[T1, T2, T3, T4, T5]]:
    def unapply(x: Expr[Tuple5[T1, T2, T3, T4, T5]])(using Quotes): Option[Tuple5[T1, T2, T3, T4, T5]] =
      FromExprFactory.tuple5FromExprFactory[T1, T2, T3, T4, T5].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple5FromExpr[T1, T2, T3, T4, T5](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5]): Tuple5FromExpr[T1, T2, T3, T4, T5] = new Tuple5FromExpr[T1, T2, T3, T4, T5]

  /** Default implementation of `FromExpr[Tuple6[...]]`
   *  - Transform `'{Tuple6(x1, ..., x6)}` into `Some(Tuple6(x1, ..., x6))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple6FromExpr[T1, T2, T3, T4, T5, T6] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6]) extends FromExpr[Tuple6[T1, T2, T3, T4, T5, T6]]:
    def unapply(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]])(using Quotes): Option[Tuple6[T1, T2, T3, T4, T5, T6]] =
      FromExprFactory.tuple6FromExprFactory[T1, T2, T3, T4, T5, T6].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple6FromExpr[T1, T2, T3, T4, T5, T6](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6]): Tuple6FromExpr[T1, T2, T3, T4, T5, T6] = new Tuple6FromExpr[T1, T2, T3, T4, T5, T6]

  /** Default implementation of `FromExpr[Tuple7[...]]`
   *  - Transform `'{Tuple7(x1, ..., x7)}` into `Some(Tuple7(x1, ..., x7))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple7FromExpr[T1, T2, T3, T4, T5, T6, T7] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7]) extends FromExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
    def unapply(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]])(using Quotes): Option[Tuple7[T1, T2, T3, T4, T5, T6, T7]] =
      FromExprFactory.tuple7FromExprFactory[T1, T2, T3, T4, T5, T6, T7].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple7FromExpr[T1, T2, T3, T4, T5, T6, T7](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7]): Tuple7FromExpr[T1, T2, T3, T4, T5, T6, T7] = new Tuple7FromExpr[T1, T2, T3, T4, T5, T6, T7]

  /** Default implementation of `FromExpr[Tuple8[...]]`
   *  - Transform `'{Tuple8(x1, ..., x8)}` into `Some(Tuple8(x1, ..., x8))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple8FromExpr[T1, T2, T3, T4, T5, T6, T7, T8] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8]) extends FromExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
    def unapply(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]])(using Quotes): Option[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] =
      FromExprFactory.tuple8FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple8FromExpr[T1, T2, T3, T4, T5, T6, T7, T8](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8]): Tuple8FromExpr[T1, T2, T3, T4, T5, T6, T7, T8] = new Tuple8FromExpr[T1, T2, T3, T4, T5, T6, T7, T8]

  /** Default implementation of `FromExpr[Tuple9[...]]`
   *  - Transform `'{Tuple9(x1, ..., x9)}` into `Some(Tuple9(x1, ..., x9))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple9FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9]) extends FromExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
    def unapply(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]])(using Quotes): Option[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
      FromExprFactory.tuple9FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple9FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9]): Tuple9FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new Tuple9FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9]

  /** Default implementation of `FromExpr[Tuple10[...]]`
   *  - Transform `'{Tuple0(x1, ..., x10)}` into `Some(Tuple0(x1, ..., x10))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple10FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10]) extends FromExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
    def unapply(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]])(using Quotes): Option[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
      FromExprFactory.tuple10FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple10FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10]): Tuple10FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new Tuple10FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]

  /** Default implementation of `FromExpr[Tuple11[...]]`
   *  - Transform `'{Tuple1(x1, ..., x11)}` into `Some(Tuple1(x1, ..., x11))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple11FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11]) extends FromExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
    def unapply(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]])(using Quotes): Option[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
      FromExprFactory.tuple11FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple11FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11]): Tuple11FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new Tuple11FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]

  /** Default implementation of `FromExpr[Tuple12[...]]`
   *  - Transform `'{Tuple2(x1, ..., x12)}` into `Some(Tuple2(x1, ..., x12))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple12FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12]) extends FromExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
    def unapply(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]])(using Quotes): Option[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
      FromExprFactory.tuple12FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple12FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12]): Tuple12FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new Tuple12FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]

  /** Default implementation of `FromExpr[Tuple13[...]]`
   *  - Transform `'{Tuple3(x1, ..., x13)}` into `Some(Tuple3(x1, ..., x13))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple13FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13]) extends FromExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
    def unapply(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]])(using Quotes): Option[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
      FromExprFactory.tuple13FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple13FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13]): Tuple13FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new Tuple13FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]

  /** Default implementation of `FromExpr[Tuple14[...]]`
   *  - Transform `'{Tuple4(x1, ..., x14)}` into `Some(Tuple4(x1, ..., x14))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple14FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14]) extends FromExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
    def unapply(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]])(using Quotes): Option[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
      FromExprFactory.tuple14FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple14FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14]): Tuple14FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new Tuple14FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]

  /** Default implementation of `FromExpr[Tuple15[...]]`
   *  - Transform `'{Tuple5(x1, ..., x15)}` into `Some(Tuple5(x1, ..., x15))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple15FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15]) extends FromExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
    def unapply(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]])(using Quotes): Option[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
      FromExprFactory.tuple15FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple15FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15]): Tuple15FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new Tuple15FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]

  /** Default implementation of `FromExpr[Tuple16[...]]`
   *  - Transform `'{Tuple6(x1, ..., x16)}` into `Some(Tuple6(x1, ..., x16))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple16FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16]) extends FromExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
    def unapply(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]])(using Quotes): Option[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] =
      FromExprFactory.tuple16FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple16FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16]): Tuple16FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new Tuple16FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]

  /** Default implementation of `FromExpr[Tuple17[...]]`
   *  - Transform `'{Tuple7(x1, ..., x17)}` into `Some(Tuple7(x1, ..., x17))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple17FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17]) extends FromExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
    def unapply(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]])(using Quotes): Option[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] =
      FromExprFactory.tuple17FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple17FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17]): Tuple17FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new Tuple17FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]

  /** Default implementation of `FromExpr[Tuple18[...]]`
   *  - Transform `'{Tuple8(x1, ..., x18)}` into `Some(Tuple8(x1, ..., x18))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple18FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18]) extends FromExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
    def unapply(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]])(using Quotes): Option[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] =
      FromExprFactory.tuple18FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple18FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18]): Tuple18FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new Tuple18FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]

  /** Default implementation of `FromExpr[Tuple19[...]]`
   *  - Transform `'{Tuple9(x1, ..., x19)}` into `Some(Tuple9(x1, ..., x19))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple19FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19]) extends FromExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
    def unapply(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]])(using Quotes): Option[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] =
      FromExprFactory.tuple19FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple19FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19]): Tuple19FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new Tuple19FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]

  /** Default implementation of `FromExpr[Tuple20[...]]`
   *  - Transform `'{Tuple0(x1, ..., x20)}` into `Some(Tuple0(x1, ..., x20))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple20FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20]) extends FromExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
    def unapply(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]])(using Quotes): Option[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] =
      FromExprFactory.tuple20FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple20FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20]): Tuple20FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new Tuple20FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]

  /** Default implementation of `FromExpr[Tuple21[...]]`
   *  - Transform `'{Tuple1(x1, ..., x21)}` into `Some(Tuple1(x1, ..., x21))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple21FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20], FromExpr[T21]) extends FromExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
    def unapply(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]])(using Quotes): Option[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] =
      FromExprFactory.tuple21FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple21FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20], FromExpr[T21]): Tuple21FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new Tuple21FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]

  /** Default implementation of `FromExpr[Tuple22[...]]`
   *  - Transform `'{Tuple2(x1, ..., x22)}` into `Some(Tuple2(x1, ..., x22))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Otherwise returns `None`
   */
  class Tuple22FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] @publicInBinary private[quoted] (using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20], FromExpr[T21], FromExpr[T22]) extends FromExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
    def unapply(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]])(using Quotes): Option[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] =
      FromExprFactory.tuple22FromExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22].apply().unapply(x)

  @publicInBinary private[quoted] final def Tuple22FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](using Type[T1], Type[T2], Type[T3], Type[T4], Type[T5], Type[T6], Type[T7], Type[T8], Type[T9], Type[T10], Type[T11], Type[T12], Type[T13], Type[T14], Type[T15], Type[T16], Type[T17], Type[T18], Type[T19], Type[T20], Type[T21], Type[T22], FromExpr[T1], FromExpr[T2], FromExpr[T3], FromExpr[T4], FromExpr[T5], FromExpr[T6], FromExpr[T7], FromExpr[T8], FromExpr[T9], FromExpr[T10], FromExpr[T11], FromExpr[T12], FromExpr[T13], FromExpr[T14], FromExpr[T15], FromExpr[T16], FromExpr[T17], FromExpr[T18], FromExpr[T19], FromExpr[T20], FromExpr[T21], FromExpr[T22]): Tuple22FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new Tuple22FromExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]

  /** Default implementation of `FromExpr[Seq]`
   *  - Transform `'{Seq(x1, ..., xn)}` into `Some(Seq(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform sequences that come out of varargs
   *  - Otherwise returns `None`
   */
  class SeqFromExpr[T] @publicInBinary private[quoted] (using Type[T], FromExpr[T]) extends FromExpr[Seq[T]]:
    def unapply(x: Expr[Seq[T]])(using Quotes): Option[Seq[T]] =
      FromExprFactory.seqFromExprFactory[T].apply().unapply(x)

  @publicInBinary private[quoted] final def SeqFromExpr[T](using Type[T], FromExpr[T]): SeqFromExpr[T] = new SeqFromExpr[T]

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
  class ListFromExpr[T] @publicInBinary private[quoted] (using Type[T], FromExpr[T]) extends FromExpr[List[T]]:
    def unapply(x: Expr[List[T]])(using Quotes): Option[List[T]] =
      FromExprFactory.listFromExprFactory[T].apply().unapply(x)

  @publicInBinary private[quoted] final def ListFromExpr[T](using Type[T], FromExpr[T]): ListFromExpr[T] = new ListFromExpr[T]

  /** Default implementation of `FromExpr[Set]`
   *  - Transform `'{Set(x1, ..., xn)}` into `Some(Set(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{Set.empty}` into `Some(Set())`
   *  - Otherwise returns `None`
   */
  class SetFromExpr[T] @publicInBinary private[quoted] (using Type[T], FromExpr[T]) extends FromExpr[Set[T]]:
    def unapply(x: Expr[Set[T]])(using Quotes): Option[Set[T]] =
      FromExprFactory.setFromExprFactory[T].apply().unapply(x)

  @publicInBinary private[quoted] final def SetFromExpr[T](using Type[T], FromExpr[T]): SetFromExpr[T] = new SetFromExpr[T]

  /** Default implementation of `FromExpr[Map]`
   *  - Transform `'{Map(x1, ..., xn)}` into `Some(Map(x1, ..., xn))` if all `xi` can be transformed using `FromExpr[Ti]`
   *  - Transform `'{Map.empty}` into `Some(Map())`
   *  - Otherwise returns `None`
   */
  class MapFromExpr[T, U] @publicInBinary private[quoted] (using Type[T], Type[U], FromExpr[T], FromExpr[U]) extends FromExpr[Map[T, U]]:
    def unapply(x: Expr[Map[T, U]])(using Quotes): Option[Map[T, U]] =
      FromExprFactory.mapFromExprFactory[T, U].apply().unapply(x)

  @publicInBinary private[quoted] final def MapFromExpr[T, U](using Type[T], Type[U], FromExpr[T], FromExpr[U]): MapFromExpr[T, U] = new MapFromExpr[T, U]

  /** Default implementation of `FromExpr[Either]`
   *  - Transform `'{Left(x)}` into `Some(Left(x))` if `x` can be transformed using `FromExpr[L]`
   *  - Transform `'{Right(x)}` into `Some(Right(x))` if `x` can be transformed using `FromExpr[R]`
   *  - Otherwise returns `None`
   */
  class EitherFromExpr[L, R] @publicInBinary private[quoted] (using Type[L], Type[R], FromExpr[L], FromExpr[R]) extends FromExpr[Either[L, R]]:
    def unapply(x: Expr[Either[L, R]])(using Quotes): Option[Either[L, R]] =
      FromExprFactory.eitherFromExprFactory[L, R].apply().unapply(x)

  @publicInBinary private[quoted] final def EitherFromExpr[L, R](using Type[L], Type[R], FromExpr[L], FromExpr[R]): EitherFromExpr[L, R] = new EitherFromExpr[L, R]

  /** Default implementation of `FromExpr[Left]`
   *  - Transform `'{Left(x)}` into `Some(Left(x))` if `x` can be transformed using `FromExpr[L]`
   *  - Otherwise returns `None`
   */
  class LeftFromExpr[L, R] @publicInBinary private[quoted] (using Type[L], Type[R], FromExpr[L]) extends FromExpr[Left[L, R]]:
    def unapply(x: Expr[Left[L, R]])(using Quotes): Option[Left[L, R]] =
      FromExprFactory.leftFromExprFactory[L, R].apply().unapply(x)

  @publicInBinary private[quoted] final def LeftFromExpr[L, R](using Type[L], Type[R], FromExpr[L]): LeftFromExpr[L, R] = new LeftFromExpr[L, R]

  /** Default implementation of `FromExpr[Right]`
   *  - Transform `'{Right(x)}` into `Some(Right(x))` if `x` can be transformed using `FromExpr[R]`
   *  - Otherwise returns `None`
   */
  class RightFromExpr[L, R] @publicInBinary private[quoted] (using Type[L], Type[R], FromExpr[R]) extends FromExpr[Right[L, R]]:
    def unapply(x: Expr[Right[L, R]])(using Quotes): Option[Right[L, R]] =
      FromExprFactory.rightFromExprFactory[L, R].apply().unapply(x)

  @publicInBinary private[quoted] final def RightFromExpr[L, R](using Type[L], Type[R], FromExpr[R]): RightFromExpr[L, R] = new RightFromExpr[L, R]

}
