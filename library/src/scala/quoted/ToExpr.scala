package scala.quoted

import language.experimental.captureChecking
import scala.annotation.nowarn
import scala.reflect.ClassTag

/** A type class for types that can convert a value of `T` into `quoted.Expr[T]`
 * an expression that, when evaluated, produces a value equal to the original.
 *
 * @tparam T the type of the value to be lifted into an `Expr[T]`
 */
trait ToExpr[T] {

  /** Lift a value into an expression containing the construction of that value.
   *
   * @param x the value to lift into a quoted expression
   * @return an `Expr[T]` that, when evaluated, produces a value equal to `x`
   */
  def apply(x: T)(using Quotes): Expr[T]

}

// Lowest priority: only used when nothing more specific (including a derived
// ToExprFactory instance) is found, e.g. a non-case singleton like `object Marker`.
private[quoted] trait LowestPriorityToExpr:
  given ValueOfToExpr: [T: {ValueOf, Type}] => ToExpr[T]:
    def apply(x: T)(using Quotes): Expr[T] = '{ valueOf[T] }

private[quoted] trait LowPriorityToExpr extends LowestPriorityToExpr:
  given fromFactory: [T: Type] => (f: ToExprFactory[T]) => ToExpr[T] = f.apply()

/** Default given instances of `ToExpr`. */
object ToExpr extends LowPriorityToExpr {

  // IMPORTANT Keep in sync with tests/run-staging/liftables.scala

  /** Default implementation of `ToExpr[Boolean]`. */
  given BooleanToExpr[T <: Boolean]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(BooleanConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Byte]`. */
  given ByteToExpr[T <: Byte]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(ByteConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Short]`. */
  given ShortToExpr[T <: Short]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(ShortConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Int]`. */
  given IntToExpr[T <: Int]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(IntConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Long]`. */
  given LongToExpr[T <: Long]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(LongConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Float]`. */
  given FloatToExpr[T <: Float]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(FloatConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Double]`. */
  given DoubleToExpr[T <: Double]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(DoubleConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Char]`. */
  given CharToExpr[T <: Char]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(CharConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[String]`. */
  given StringToExpr[T <: String]: ToExpr[T] with {
    def apply(x: T)(using Quotes) =
      import quotes.reflect.*
      Literal(StringConstant(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default implementation of `ToExpr[Class[T]]`. */
  given ClassToExpr[T <: Class[?]]: ToExpr[T] with {
    def apply(x: T)(using Quotes) = {
      import quotes.reflect.*
      Ref(defn.Predef_classOf).appliedToType(TypeRepr.typeConstructorOf(x)).asExpr.asInstanceOf[Expr[T]]
    }
  }

  /** Default implementation of `ToExpr[ClassTag[T]]`. Proxies to `ToExprFactory.classTagToExprFactory`. */
  given ClassTagToExpr: [T: Type] => (factory: ToExprFactory[ClassTag[T]]) => ToExpr[ClassTag[T]]:
    def apply(ct: ClassTag[T])(using Quotes): Expr[ClassTag[T]] = factory.apply().apply(ct)

  /** Default implementation of `ToExpr[Array[T]]`. Proxies to `ToExprFactory.arrayToExprFactory`. */
  given ArrayToExpr: [T: {Type, ToExpr, ClassTag}] => (factory: ToExprFactory[Array[T]]) => ToExpr[Array[T]]:
    def apply(arr: Array[T])(using Quotes): Expr[Array[T]] = factory.apply().apply(arr)

  /** Default implementation of `ToExpr[Array[Boolean]]`. */
  given ArrayOfBooleanToExpr: ToExpr[Array[Boolean]] with {
    def apply(array: Array[Boolean])(using Quotes): Expr[Array[Boolean]] =
      if (array.length == 0) '{ Array.emptyBooleanArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Byte]]`. */
  given ArrayOfByteToExpr: ToExpr[Array[Byte]] with {
    def apply(array: Array[Byte])(using Quotes): Expr[Array[Byte]] =
      if (array.length == 0) '{ Array.emptyByteArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Short]]`. */
  given ArrayOfShortToExpr: ToExpr[Array[Short]] with {
    def apply(array: Array[Short])(using Quotes): Expr[Array[Short]] =
      if (array.length == 0) '{ Array.emptyShortArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Char]]`. */
  given ArrayOfCharToExpr: ToExpr[Array[Char]] with {
    def apply(array: Array[Char])(using Quotes): Expr[Array[Char]] =
      if (array.length == 0) '{ Array.emptyCharArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Int]]`. */
  given ArrayOfIntToExpr: ToExpr[Array[Int]] with {
    def apply(array: Array[Int])(using Quotes): Expr[Array[Int]] =
      if (array.length == 0) '{ Array.emptyIntArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Long]]`. */
  given ArrayOfLongToExpr: ToExpr[Array[Long]] with {
    def apply(array: Array[Long])(using Quotes): Expr[Array[Long]] =
      if (array.length == 0) '{ Array.emptyLongArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Float]]`. */
  given ArrayOfFloatToExpr: ToExpr[Array[Float]] with {
    def apply(array: Array[Float])(using Quotes): Expr[Array[Float]] =
      if (array.length == 0) '{ Array.emptyFloatArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[Array[Double]]`. */
  given ArrayOfDoubleToExpr: ToExpr[Array[Double]] with {
    def apply(array: Array[Double])(using Quotes): Expr[Array[Double]] =
      if (array.length == 0) '{ Array.emptyDoubleArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}*) }
  }

  /** Default implementation of `ToExpr[IArray[T]]`. */
  given IArrayToExpr[T: Type](using ltArray: ToExpr[Array[T]]): ToExpr[IArray[T]] with {
    def apply(iarray: IArray[T])(using Quotes): Expr[IArray[T]] =
      '{ ${ltArray.apply(iarray.asInstanceOf[Array[T]])}.asInstanceOf[IArray[T]] }
  }

  /** Default implementation of `ToExpr[Seq[T]]`. Proxies to `ToExprFactory.seqToExprFactory`. */
  given SeqToExpr: [T: {Type, ToExpr}] => (factory: ToExprFactory[Seq[T]]) => ToExpr[Seq[T]]:
    def apply(xs: Seq[T])(using Quotes): Expr[Seq[T]] = factory.apply().apply(xs)

  /** Default implementation of `ToExpr[List[T]]`. Proxies to `ToExprFactory.listToExprFactory`. */
  given ListToExpr: [T: {Type, ToExpr}] => (factory: ToExprFactory[List[T]]) => ToExpr[List[T]]:
    def apply(xs: List[T])(using Quotes): Expr[List[T]] = factory.apply().apply(xs)

  /** Default implementation of `ToExpr[Nil.type]`. */
  given NilToExpr: ToExpr[Nil.type] with {
    def apply(xs: Nil.type)(using Quotes): Expr[Nil.type] =
      '{ Nil }
  }

  /** Default implementation of `ToExpr[Set[T]]`. Proxies to `ToExprFactory.setToExprFactory`. */
  given SetToExpr: [T: {Type, ToExpr}] => (factory: ToExprFactory[Set[T]]) => ToExpr[Set[T]]:
    def apply(xs: Set[T])(using Quotes): Expr[Set[T]] = factory.apply().apply(xs)

  /** Default implementation of `ToExpr[Map[T, U]]`. Proxies to `ToExprFactory.mapToExprFactory`. */
  given MapToExpr: [T: {Type, ToExpr}, U: {Type, ToExpr}] => (factory: ToExprFactory[Map[T, U]]) => ToExpr[Map[T, U]]:
    def apply(m: Map[T, U])(using Quotes): Expr[Map[T, U]] = factory.apply().apply(m)

  /** Default implementation of `ToExpr[Option[T]]`. Proxies to `ToExprFactory.optionToExprFactory`. */
  given OptionToExpr: [T: {Type, ToExpr}] => (factory: ToExprFactory[Option[T]]) => ToExpr[Option[T]]:
    def apply(x: Option[T])(using Quotes): Expr[Option[T]] = factory.apply().apply(x)

  /** Default implementation of `ToExpr[Some[T]]`. Proxies to `ToExprFactory.someToExprFactory`. */
  given SomeToExpr: [T: {Type, ToExpr}] => (factory: ToExprFactory[Some[T]]) => ToExpr[Some[T]]:
    def apply(x: Some[T])(using Quotes): Expr[Some[T]] = factory.apply().apply(x)

  /** Default implementation of `ToExpr[None.type]`. */
  given NoneToExpr: ToExpr[None.type] with {
    def apply(x: None.type)(using Quotes): Expr[None.type] =
      '{ None }
  }

  /** Default implementation of `ToExpr[Either[L, R]]`. Proxies to `ToExprFactory.eitherToExprFactory`. */
  given EitherToExpr: [L: {Type, ToExpr}, R: {Type, ToExpr}] => (factory: ToExprFactory[Either[L, R]]) => ToExpr[Either[L, R]]:
    def apply(x: Either[L, R])(using Quotes): Expr[Either[L, R]] = factory.apply().apply(x)

  /** Default implementation of `ToExpr[Left[L, R]]`. Proxies to `ToExprFactory.leftToExprFactory`. */
  given LeftToExpr: [L: {Type, ToExpr}, R: Type] => (factory: ToExprFactory[Left[L, R]]) => ToExpr[Left[L, R]]:
    def apply(x: Left[L, R])(using Quotes): Expr[Left[L, R]] = factory.apply().apply(x)

  /** Default implementation of `ToExpr[Right[L, R]]`. Proxies to `ToExprFactory.rightToExprFactory`. */
  given RightToExpr: [L: Type, R: {Type, ToExpr}] => (factory: ToExprFactory[Right[L, R]]) => ToExpr[Right[L, R]]:
    def apply(x: Right[L, R])(using Quotes): Expr[Right[L, R]] = factory.apply().apply(x)

  /** Default implementation of `ToExpr[EmptyTuple.type]`. */
  given EmptyTupleToExpr: ToExpr[EmptyTuple.type] with {
    def apply(tup: EmptyTuple.type)(using Quotes) =
      '{ EmptyTuple }
  }

  /** Default implementation of `ToExpr[Tuple1[T1]]`. Proxies to `ToExprFactory.tuple1ToExprFactory`. */
  given Tuple1ToExpr: [T1: {Type, ToExpr}] => (factory: ToExprFactory[Tuple1[T1]]) => ToExpr[Tuple1[T1]]:
    def apply(tup: Tuple1[T1])(using Quotes): Expr[Tuple1[T1]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple2[T1, T2]]`. Proxies to `ToExprFactory.tuple2ToExprFactory`. */
  given Tuple2ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}] => (factory: ToExprFactory[Tuple2[T1, T2]]) => ToExpr[Tuple2[T1, T2]]:
    def apply(tup: Tuple2[T1, T2])(using Quotes): Expr[Tuple2[T1, T2]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple3[T1, T2, T3]]`. Proxies to `ToExprFactory.tuple3ToExprFactory`. */
  given Tuple3ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}] => (factory: ToExprFactory[Tuple3[T1, T2, T3]]) => ToExpr[Tuple3[T1, T2, T3]]:
    def apply(tup: Tuple3[T1, T2, T3])(using Quotes): Expr[Tuple3[T1, T2, T3]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple4[T1, T2, T3, T4]]`. Proxies to `ToExprFactory.tuple4ToExprFactory`. */
  given Tuple4ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}] => (factory: ToExprFactory[Tuple4[T1, T2, T3, T4]]) => ToExpr[Tuple4[T1, T2, T3, T4]]:
    def apply(tup: Tuple4[T1, T2, T3, T4])(using Quotes): Expr[Tuple4[T1, T2, T3, T4]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple5[T1, T2, T3, T4, T5]]`. Proxies to `ToExprFactory.tuple5ToExprFactory`. */
  given Tuple5ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}] => (factory: ToExprFactory[Tuple5[T1, T2, T3, T4, T5]]) => ToExpr[Tuple5[T1, T2, T3, T4, T5]]:
    def apply(tup: Tuple5[T1, T2, T3, T4, T5])(using Quotes): Expr[Tuple5[T1, T2, T3, T4, T5]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]]`. Proxies to `ToExprFactory.tuple6ToExprFactory`. */
  given Tuple6ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}] => (factory: ToExprFactory[Tuple6[T1, T2, T3, T4, T5, T6]]) => ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]]:
    def apply(tup: Tuple6[T1, T2, T3, T4, T5, T6])(using Quotes): Expr[Tuple6[T1, T2, T3, T4, T5, T6]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]`. Proxies to `ToExprFactory.tuple7ToExprFactory`. */
  given Tuple7ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}] => (factory: ToExprFactory[Tuple7[T1, T2, T3, T4, T5, T6, T7]]) => ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
    def apply(tup: Tuple7[T1, T2, T3, T4, T5, T6, T7])(using Quotes): Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]`. Proxies to `ToExprFactory.tuple8ToExprFactory`. */
  given Tuple8ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}] => (factory: ToExprFactory[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]) => ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
    def apply(tup: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8])(using Quotes): Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]`. Proxies to `ToExprFactory.tuple9ToExprFactory`. */
  given Tuple9ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}] => (factory: ToExprFactory[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]) => ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
    def apply(tup: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9])(using Quotes): Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]`. Proxies to `ToExprFactory.tuple10ToExprFactory`. */
  given Tuple10ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}] => (factory: ToExprFactory[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]) => ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
    def apply(tup: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10])(using Quotes): Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]`. Proxies to `ToExprFactory.tuple11ToExprFactory`. */
  given Tuple11ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}] => (factory: ToExprFactory[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]) => ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
    def apply(tup: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11])(using Quotes): Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]`. Proxies to `ToExprFactory.tuple12ToExprFactory`. */
  given Tuple12ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}] => (factory: ToExprFactory[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]) => ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
    def apply(tup: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12])(using Quotes): Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]`. Proxies to `ToExprFactory.tuple13ToExprFactory`. */
  given Tuple13ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}] => (factory: ToExprFactory[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]) => ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
    def apply(tup: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13])(using Quotes): Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]`. Proxies to `ToExprFactory.tuple14ToExprFactory`. */
  given Tuple14ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}] => (factory: ToExprFactory[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]) => ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
    def apply(tup: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14])(using Quotes): Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]`. Proxies to `ToExprFactory.tuple15ToExprFactory`. */
  given Tuple15ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}] => (factory: ToExprFactory[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]) => ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
    def apply(tup: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15])(using Quotes): Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]`. Proxies to `ToExprFactory.tuple16ToExprFactory`. */
  given Tuple16ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}] => (factory: ToExprFactory[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]) => ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
    def apply(tup: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16])(using Quotes): Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]`. Proxies to `ToExprFactory.tuple17ToExprFactory`. */
  given Tuple17ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}] => (factory: ToExprFactory[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]) => ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
    def apply(tup: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17])(using Quotes): Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]`. Proxies to `ToExprFactory.tuple18ToExprFactory`. */
  given Tuple18ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}] => (factory: ToExprFactory[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]) => ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
    def apply(tup: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18])(using Quotes): Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]`. Proxies to `ToExprFactory.tuple19ToExprFactory`. */
  given Tuple19ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}] => (factory: ToExprFactory[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) => ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
    def apply(tup: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19])(using Quotes): Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]`. Proxies to `ToExprFactory.tuple20ToExprFactory`. */
  given Tuple20ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}] => (factory: ToExprFactory[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]) => ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
    def apply(tup: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20])(using Quotes): Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]`. Proxies to `ToExprFactory.tuple21ToExprFactory`. */
  given Tuple21ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}, T21: {Type, ToExpr}] => (factory: ToExprFactory[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]) => ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
    def apply(tup: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21])(using Quotes): Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]`. Proxies to `ToExprFactory.tuple22ToExprFactory`. */
  given Tuple22ToExpr: [T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}, T21: {Type, ToExpr}, T22: {Type, ToExpr}] => (factory: ToExprFactory[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]) => ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
    def apply(tup: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22])(using Quotes): Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[H *: T]`. Proxies to `ToExprFactory.tupleConsToExprFactory`. */
  given TupleConsToExpr: [H: {Type, ToExpr}, T <: Tuple: {Type, ToExpr}] => (factory: ToExprFactory[H *: T]) => ToExpr[H *: T]:
    def apply(tup: H *: T)(using Quotes): Expr[H *: T] = factory.apply().apply(tup)

  /** Default implementation of `ToExpr[BigInt]`. */
  given BigIntToExpr: ToExpr[BigInt] with {
    def apply(x: BigInt)(using Quotes): Expr[BigInt] =
      '{ BigInt(${Expr(x.toByteArray)}) }
  }

  /** Default implementation of `ToExpr[BigDecimal using the default MathContext]`. */
  given BigDecimalToExpr: ToExpr[BigDecimal] with {
    def apply(x: BigDecimal)(using Quotes): Expr[BigDecimal] =
      val bigDecimal: String = "" + x // workaround "method toString in class BigDecimal does not take parameters" in scaladoc/generateScalaDocumentation
      '{ BigDecimal(${Expr(bigDecimal)}) }
  }

  /** Default implementation of `ToExpr[StringContext]`. */
  given StringContextToExpr: ToExpr[StringContext] with {
    def apply(stringContext: StringContext)(using Quotes): Expr[StringContext] =
      val parts = Varargs(stringContext.parts.map(Expr(_)))
      '{ StringContext($parts*) }
  }

}
