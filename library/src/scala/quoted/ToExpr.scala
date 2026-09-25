package scala.quoted

import language.experimental.captureChecking

import scala.annotation.publicInBinary
import scala.reflect.ClassTag

/** A type class for types that can convert a value of `T` into `quoted.Expr[T]`
 *  an expression that, when evaluated, produces a value equal to the original.
 *
 *  @tparam T the type of the value to be lifted into an `Expr[T]`
 */
trait ToExpr[T] {

  /** Lift a value into an expression containing the construction of that value.
   *
   *  @param x the value to lift into a quoted expression
   *  @return an `Expr[T]` that, when evaluated, produces a value equal to `x`
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

  /** Default implementation of `ToExpr[ClassTag[T]]`. */
  class ClassTagToExpr[T: Type] @publicInBinary private[quoted] extends ToExpr[ClassTag[T]]:
    def apply(x: ClassTag[T])(using Quotes): Expr[ClassTag[T]] =
      ToExprFactory.classTagToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def ClassTagToExpr[T: Type]: ClassTagToExpr[T] = new ClassTagToExpr[T]

  /** Default implementation of `ToExpr[Array[T]]`. */
  class ArrayToExpr[T: {Type, ToExpr, ClassTag}] @publicInBinary private[quoted] extends ToExpr[Array[T]]:
    def apply(x: Array[T])(using Quotes): Expr[Array[T]] =
      ToExprFactory.arrayToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def ArrayToExpr[T: {Type, ToExpr, ClassTag}]: ArrayToExpr[T] = new ArrayToExpr[T]

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

  /** Default implementation of `ToExpr[Seq[T]]`. */
  class SeqToExpr[T: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Seq[T]]:
    def apply(x: Seq[T])(using Quotes): Expr[Seq[T]] =
      ToExprFactory.seqToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def SeqToExpr[T: {Type, ToExpr}]: SeqToExpr[T] = new SeqToExpr[T]

  /** Default implementation of `ToExpr[List[T]]`. */
  class ListToExpr[T: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[List[T]]:
    def apply(x: List[T])(using Quotes): Expr[List[T]] =
      ToExprFactory.listToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def ListToExpr[T: {Type, ToExpr}]: ListToExpr[T] = new ListToExpr[T]

  /** Default implementation of `ToExpr[Nil.type]`. */
  given NilToExpr: ToExpr[Nil.type] with {
    def apply(xs: Nil.type)(using Quotes): Expr[Nil.type] =
      '{ Nil }
  }

  /** Default implementation of `ToExpr[Set[T]]`. */
  class SetToExpr[T: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Set[T]]:
    def apply(x: Set[T])(using Quotes): Expr[Set[T]] =
      ToExprFactory.setToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def SetToExpr[T: {Type, ToExpr}]: SetToExpr[T] = new SetToExpr[T]

  /** Default implementation of `ToExpr[Map[T, U]]`. */
  class MapToExpr[T: {Type, ToExpr}, U: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Map[T, U]]:
    def apply(x: Map[T, U])(using Quotes): Expr[Map[T, U]] =
      ToExprFactory.mapToExprFactory[T, U].apply().apply(x)

  @publicInBinary private[quoted] final def MapToExpr[T: {Type, ToExpr}, U: {Type, ToExpr}]: MapToExpr[T, U] = new MapToExpr[T, U]

  /** Default implementation of `ToExpr[Option[T]]`. */
  class OptionToExpr[T: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Option[T]]:
    def apply(x: Option[T])(using Quotes): Expr[Option[T]] =
      ToExprFactory.optionToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def OptionToExpr[T: {Type, ToExpr}]: OptionToExpr[T] = new OptionToExpr[T]

  /** Default implementation of `ToExpr[Some[T]]`. */
  class SomeToExpr[T: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Some[T]]:
    def apply(x: Some[T])(using Quotes): Expr[Some[T]] =
      ToExprFactory.someToExprFactory[T].apply().apply(x)

  @publicInBinary private[quoted] final def SomeToExpr[T: {Type, ToExpr}]: SomeToExpr[T] = new SomeToExpr[T]

  /** Default implementation of `ToExpr[None.type]`. */
  given NoneToExpr: ToExpr[None.type] with {
    def apply(x: None.type)(using Quotes): Expr[None.type] =
      '{ None }
  }

  /** Default implementation of `ToExpr[Either[L, R]]`. */
  class EitherToExpr[L: {Type, ToExpr}, R: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Either[L, R]]:
    def apply(x: Either[L, R])(using Quotes): Expr[Either[L, R]] =
      ToExprFactory.eitherToExprFactory[L, R].apply().apply(x)

  @publicInBinary private[quoted] final def EitherToExpr[L: {Type, ToExpr}, R: {Type, ToExpr}]: EitherToExpr[L, R] = new EitherToExpr[L, R]

  /** Default implementation of `ToExpr[Left[L, R]]`. */
  class LeftToExpr[L: {Type, ToExpr}, R: Type] @publicInBinary private[quoted] extends ToExpr[Left[L, R]]:
    def apply(x: Left[L, R])(using Quotes): Expr[Left[L, R]] =
      ToExprFactory.leftToExprFactory[L, R].apply().apply(x)

  @publicInBinary private[quoted] final def LeftToExpr[L: {Type, ToExpr}, R: Type]: LeftToExpr[L, R] = new LeftToExpr[L, R]

  /** Default implementation of `ToExpr[Right[L, R]]`. */
  class RightToExpr[L: Type, R: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Right[L, R]]:
    def apply(x: Right[L, R])(using Quotes): Expr[Right[L, R]] =
      ToExprFactory.rightToExprFactory[L, R].apply().apply(x)

  @publicInBinary private[quoted] final def RightToExpr[L: Type, R: {Type, ToExpr}]: RightToExpr[L, R] = new RightToExpr[L, R]

  /** Default implementation of `ToExpr[EmptyTuple.type]`. */
  given EmptyTupleToExpr: ToExpr[EmptyTuple.type] with {
    def apply(tup: EmptyTuple.type)(using Quotes) =
      '{ EmptyTuple }
  }

  /** Default implementation of `ToExpr[Tuple1[T1]]`. */
  class Tuple1ToExpr[T1: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple1[T1]]:
    def apply(x: Tuple1[T1])(using Quotes): Expr[Tuple1[T1]] =
      ToExprFactory.tuple1ToExprFactory[T1].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple1ToExpr[T1: {Type, ToExpr}]: Tuple1ToExpr[T1] = new Tuple1ToExpr[T1]

  /** Default implementation of `ToExpr[Tuple2[T1, T2]]`. */
  class Tuple2ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple2[T1, T2]]:
    def apply(x: Tuple2[T1, T2])(using Quotes): Expr[Tuple2[T1, T2]] =
      ToExprFactory.tuple2ToExprFactory[T1, T2].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple2ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}]: Tuple2ToExpr[T1, T2] = new Tuple2ToExpr[T1, T2]

  /** Default implementation of `ToExpr[Tuple3[T1, T2, T3]]`. */
  class Tuple3ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple3[T1, T2, T3]]:
    def apply(x: Tuple3[T1, T2, T3])(using Quotes): Expr[Tuple3[T1, T2, T3]] =
      ToExprFactory.tuple3ToExprFactory[T1, T2, T3].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple3ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}]: Tuple3ToExpr[T1, T2, T3] = new Tuple3ToExpr[T1, T2, T3]

  /** Default implementation of `ToExpr[Tuple4[T1, T2, T3, T4]]`. */
  class Tuple4ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple4[T1, T2, T3, T4]]:
    def apply(x: Tuple4[T1, T2, T3, T4])(using Quotes): Expr[Tuple4[T1, T2, T3, T4]] =
      ToExprFactory.tuple4ToExprFactory[T1, T2, T3, T4].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple4ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}]: Tuple4ToExpr[T1, T2, T3, T4] = new Tuple4ToExpr[T1, T2, T3, T4]

  /** Default implementation of `ToExpr[Tuple5[T1, T2, T3, T4, T5]]`. */
  class Tuple5ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple5[T1, T2, T3, T4, T5]]:
    def apply(x: Tuple5[T1, T2, T3, T4, T5])(using Quotes): Expr[Tuple5[T1, T2, T3, T4, T5]] =
      ToExprFactory.tuple5ToExprFactory[T1, T2, T3, T4, T5].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple5ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}]: Tuple5ToExpr[T1, T2, T3, T4, T5] = new Tuple5ToExpr[T1, T2, T3, T4, T5]

  /** Default implementation of `ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]]`. */
  class Tuple6ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]]:
    def apply(x: Tuple6[T1, T2, T3, T4, T5, T6])(using Quotes): Expr[Tuple6[T1, T2, T3, T4, T5, T6]] =
      ToExprFactory.tuple6ToExprFactory[T1, T2, T3, T4, T5, T6].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple6ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}]: Tuple6ToExpr[T1, T2, T3, T4, T5, T6] = new Tuple6ToExpr[T1, T2, T3, T4, T5, T6]

  /** Default implementation of `ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]`. */
  class Tuple7ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
    def apply(x: Tuple7[T1, T2, T3, T4, T5, T6, T7])(using Quotes): Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] =
      ToExprFactory.tuple7ToExprFactory[T1, T2, T3, T4, T5, T6, T7].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple7ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}]: Tuple7ToExpr[T1, T2, T3, T4, T5, T6, T7] = new Tuple7ToExpr[T1, T2, T3, T4, T5, T6, T7]

  /** Default implementation of `ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]`. */
  class Tuple8ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
    def apply(x: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8])(using Quotes): Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] =
      ToExprFactory.tuple8ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple8ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}]: Tuple8ToExpr[T1, T2, T3, T4, T5, T6, T7, T8] = new Tuple8ToExpr[T1, T2, T3, T4, T5, T6, T7, T8]

  /** Default implementation of `ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]`. */
  class Tuple9ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
    def apply(x: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9])(using Quotes): Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
      ToExprFactory.tuple9ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple9ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}]: Tuple9ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new Tuple9ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9]

  /** Default implementation of `ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]`. */
  class Tuple10ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
    def apply(x: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10])(using Quotes): Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
      ToExprFactory.tuple10ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple10ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}]: Tuple10ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new Tuple10ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]

  /** Default implementation of `ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]`. */
  class Tuple11ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
    def apply(x: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11])(using Quotes): Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
      ToExprFactory.tuple11ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple11ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}]: Tuple11ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new Tuple11ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]

  /** Default implementation of `ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]`. */
  class Tuple12ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
    def apply(x: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12])(using Quotes): Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
      ToExprFactory.tuple12ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple12ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}]: Tuple12ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new Tuple12ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]

  /** Default implementation of `ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]`. */
  class Tuple13ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
    def apply(x: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13])(using Quotes): Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
      ToExprFactory.tuple13ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple13ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}]: Tuple13ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new Tuple13ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]

  /** Default implementation of `ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]`. */
  class Tuple14ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
    def apply(x: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14])(using Quotes): Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
      ToExprFactory.tuple14ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple14ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}]: Tuple14ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new Tuple14ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]

  /** Default implementation of `ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]`. */
  class Tuple15ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
    def apply(x: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15])(using Quotes): Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
      ToExprFactory.tuple15ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple15ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}]: Tuple15ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new Tuple15ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]

  /** Default implementation of `ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]`. */
  class Tuple16ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
    def apply(x: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16])(using Quotes): Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] =
      ToExprFactory.tuple16ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple16ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}]: Tuple16ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new Tuple16ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]

  /** Default implementation of `ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]`. */
  class Tuple17ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
    def apply(x: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17])(using Quotes): Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] =
      ToExprFactory.tuple17ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple17ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}]: Tuple17ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new Tuple17ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]

  /** Default implementation of `ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]`. */
  class Tuple18ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
    def apply(x: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18])(using Quotes): Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] =
      ToExprFactory.tuple18ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple18ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}]: Tuple18ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new Tuple18ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]

  /** Default implementation of `ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]`. */
  class Tuple19ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
    def apply(x: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19])(using Quotes): Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] =
      ToExprFactory.tuple19ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple19ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}]: Tuple19ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new Tuple19ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]

  /** Default implementation of `ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]`. */
  class Tuple20ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
    def apply(x: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20])(using Quotes): Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] =
      ToExprFactory.tuple20ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple20ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}]: Tuple20ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new Tuple20ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]

  /** Default implementation of `ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]`. */
  class Tuple21ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}, T21: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
    def apply(x: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21])(using Quotes): Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] =
      ToExprFactory.tuple21ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple21ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}, T21: {Type, ToExpr}]: Tuple21ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new Tuple21ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]

  /** Default implementation of `ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]`. */
  class Tuple22ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}, T21: {Type, ToExpr}, T22: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
    def apply(x: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22])(using Quotes): Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] =
      ToExprFactory.tuple22ToExprFactory[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22].apply().apply(x)

  @publicInBinary private[quoted] final def Tuple22ToExpr[T1: {Type, ToExpr}, T2: {Type, ToExpr}, T3: {Type, ToExpr}, T4: {Type, ToExpr}, T5: {Type, ToExpr}, T6: {Type, ToExpr}, T7: {Type, ToExpr}, T8: {Type, ToExpr}, T9: {Type, ToExpr}, T10: {Type, ToExpr}, T11: {Type, ToExpr}, T12: {Type, ToExpr}, T13: {Type, ToExpr}, T14: {Type, ToExpr}, T15: {Type, ToExpr}, T16: {Type, ToExpr}, T17: {Type, ToExpr}, T18: {Type, ToExpr}, T19: {Type, ToExpr}, T20: {Type, ToExpr}, T21: {Type, ToExpr}, T22: {Type, ToExpr}]: Tuple22ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new Tuple22ToExpr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]

  /** Default implementation of `ToExpr[H *: T]`. */
  class TupleConsToExpr [H: {Type, ToExpr}, T <: Tuple: {Type, ToExpr}] @publicInBinary private[quoted] extends ToExpr[H *: T]:
    def apply(x: H *: T)(using Quotes): Expr[H *: T] =
      ToExprFactory.tupleConsToExprFactory[H, T].apply().apply(x)

  @publicInBinary private[quoted] final def TupleConsToExpr [H: {Type, ToExpr}, T <: Tuple: {Type, ToExpr}]: TupleConsToExpr[H, T] = new TupleConsToExpr[H, T]

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
