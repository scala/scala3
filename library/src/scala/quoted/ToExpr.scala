package scala.quoted

import language.experimental.captureChecking

import scala.reflect.ClassTag

/** A type class for types that can convert a value of `T` into `quoted.Expr[T]`
 *  an expression that will create a copy of the value.
 *
 *  @tparam T the type of the value to be lifted into an `Expr[T]`
 */
trait ToExpr[T] {

  /** Lift a value into an expression containing the construction of that value.
   *
   *  @param x the value to lift into a quoted expression
   */
  def apply(x: T)(using Quotes): Expr[T]

}

/** Default given instances of `ToExpr`. */
object ToExpr {

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
  given ClassTagToExpr[T: Type]: ToExpr[ClassTag[T]] with {
    def apply(ct: ClassTag[T])(using Quotes): Expr[ClassTag[T]] =
      '{ ClassTag[T](${Expr(ct.runtimeClass.asInstanceOf[Class[T]])}) }
  }

  /** Default implementation of `ToExpr[Array[T]]`. */
  given ArrayToExpr[T: Type: ToExpr: ClassTag]: ToExpr[Array[T]] with {
    def apply(arr: Array[T])(using Quotes): Expr[Array[T]] =
      '{ Array[T](${Expr(arr.toSeq)}*)(using ${Expr(summon[ClassTag[T]])}) }
  }

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
  given SeqToExpr[T: Type: ToExpr]: ToExpr[Seq[T]] with {
    def apply(xs: Seq[T])(using Quotes): Expr[Seq[T]] =
      Expr.ofSeq(xs.map(summon[ToExpr[T]].apply))
  }

  /** Default implementation of `ToExpr[List[T]]`. */
  given ListToExpr[T: Type: ToExpr]: ToExpr[List[T]] with {
    def apply(xs: List[T])(using Quotes): Expr[List[T]] =
      Expr.ofList(xs.map(summon[ToExpr[T]].apply))
  }

  /** Default implementation of `ToExpr[Nil.type]`. */
  given NilToExpr: ToExpr[Nil.type] with {
    def apply(xs: Nil.type)(using Quotes): Expr[Nil.type] =
      '{ Nil }
  }

  /** Default implementation of `ToExpr[Set[T]]`. */
  given SetToExpr[T: Type: ToExpr]: ToExpr[Set[T]] with {
    def apply(set: Set[T])(using Quotes): Expr[Set[T]] =
      '{ Set(${Expr(set.toSeq)}*) }
  }

  /** Default implementation of `ToExpr[Map[T, U]]`. */
  given MapToExpr[T: Type: ToExpr, U: Type: ToExpr]: ToExpr[Map[T, U]] with {
    def apply(map: Map[T, U])(using Quotes): Expr[Map[T, U]] =
    '{ Map(${Expr(map.toSeq)}*) }
  }

  /** Default implementation of `ToExpr[Option[T]]`. */
  given OptionToExpr[T: Type: ToExpr]: ToExpr[Option[T]] with {
    def apply(x: Option[T])(using Quotes): Expr[Option[T]] = x match {
      case x: Some[T] => Expr(x)
      case None => Expr(None)
    }
  }

  /** Default implementation of `ToExpr[Some[T]]`. */
  given SomeToExpr[T: Type: ToExpr]: ToExpr[Some[T]] with {
    def apply(x: Some[T])(using Quotes): Expr[Some[T]] =
      '{ Some[T](${Expr(x.get)}) }
  }

  /** Default implementation of `ToExpr[None.type]`. */
  given NoneToExpr: ToExpr[None.type] with {
    def apply(x: None.type)(using Quotes): Expr[None.type] =
      '{ None }
  }

  /** Default implementation of `ToExpr[Either[L, R]]`. */
  given EitherToExpr[L: Type: ToExpr, R: Type: ToExpr]: ToExpr[Either[L, R]] with {
    def apply(x: Either[L, R])(using Quotes): Expr[Either[L, R]] = x match
      case x: Left[L, R] => Expr(x)
      case x: Right[L, R] => Expr(x)
  }

  /** Default implementation of `ToExpr[Left[L, R]]`. */
  given LeftToExpr[L: Type: ToExpr, R: Type]: ToExpr[Left[L, R]] with {
    def apply(x: Left[L, R])(using Quotes): Expr[Left[L, R]] =
      '{ Left[L, R](${Expr(x.value)}) }
  }

  /** Default implementation of `ToExpr[Right[L, R]]`. */
  given RightToExpr[L: Type, R: Type: ToExpr]: ToExpr[Right[L, R]] with {
    def apply(x: Right[L, R])(using Quotes): Expr[Right[L, R]] =
      '{ Right[L, R](${Expr(x.value)}) }
  }

  /** Default implementation of `ToExpr[EmptyTuple.type]`. */
  given EmptyTupleToExpr: ToExpr[EmptyTuple.type] with {
    def apply(tup: EmptyTuple.type)(using Quotes) =
      '{ EmptyTuple }
  }

  /** Default implementation of `ToExpr[Tuple1[T1]]`. */
  given Tuple1ToExpr[T1: Type: ToExpr]: ToExpr[Tuple1[T1]] with {
    def apply(tup: Tuple1[T1])(using Quotes) =
      '{ Tuple1(${Expr(tup._1)}) }
  }

  /** Default implementation of `ToExpr[Tuple2[T1, T2]]`. */
  given Tuple2ToExpr[T1: Type: ToExpr, T2: Type: ToExpr]: ToExpr[Tuple2[T1, T2]] with {
    def apply(tup: Tuple2[T1, T2])(using Quotes) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}) }
  }

  /** Default implementation of `ToExpr[Tuple3[T1, T2, T3]]`. */
  given Tuple3ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr]: ToExpr[Tuple3[T1, T2, T3]] with {
    def apply(tup: Tuple3[T1, T2, T3])(using Quotes) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}) }
  }

  /** Default implementation of `ToExpr[Tuple4[T1, T2, T3, T4]]`. */
  given Tuple4ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr]: ToExpr[Tuple4[T1, T2, T3, T4]] with {
    def apply(tup: Tuple4[T1, T2, T3, T4])(using Quotes) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}) }
  }

  /** Default implementation of `ToExpr[Tuple5[T1, T2, T3, T4, T5]]`. */
  given Tuple5ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr]: ToExpr[Tuple5[T1, T2, T3, T4, T5]] with {
    def apply(tup: Tuple5[T1, T2, T3, T4, T5])(using Quotes) = {
      val (x1, x2, x3, x4, x5) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]]`. */
  given Tuple6ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr]: ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]] with {
    def apply(tup: Tuple6[T1, T2, T3, T4, T5, T6])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]`. */
  given Tuple7ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr]: ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] with {
    def apply(tup: Tuple7[T1, T2, T3, T4, T5, T6, T7])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]`. */
  given Tuple8ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr]: ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] with {
    def apply(tup: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]`. */
  given Tuple9ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr]: ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] with {
    def apply(tup: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]`. */
  given Tuple10ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr]: ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] with {
    def apply(tup: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]`. */
  given Tuple11ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr]: ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] with {
    def apply(tup: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]`. */
  given Tuple12ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr]: ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] with {
    def apply(tup: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]`. */
  given Tuple13ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr]: ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] with {
    def apply(tup: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]`. */
  given Tuple14ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr]: ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] with {
    def apply(tup: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]`. */
  given Tuple15ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr]: ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] with {
    def apply(tup: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]`. */
  given Tuple16ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr]: ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] with {
    def apply(tup: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]`. */
  given Tuple17ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr, T17: Type: ToExpr]: ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] with {
    def apply(tup: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]`. */
  given Tuple18ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr, T17: Type: ToExpr, T18: Type: ToExpr]: ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] with {
    def apply(tup: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]`. */
  given Tuple19ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr, T17: Type: ToExpr, T18: Type: ToExpr, T19: Type: ToExpr]: ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] with {
    def apply(tup: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]`. */
  given Tuple20ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr, T17: Type: ToExpr, T18: Type: ToExpr, T19: Type: ToExpr, T20: Type: ToExpr]: ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] with {
    def apply(tup: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]`. */
  given Tuple21ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr, T17: Type: ToExpr, T18: Type: ToExpr, T19: Type: ToExpr, T20: Type: ToExpr, T21: Type: ToExpr]: ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] with {
    def apply(tup: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}, ${Expr(x21)}) }
    }
  }

  /** Default implementation of `ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]`. */
  given Tuple22ToExpr[T1: Type: ToExpr, T2: Type: ToExpr, T3: Type: ToExpr, T4: Type: ToExpr, T5: Type: ToExpr, T6: Type: ToExpr, T7: Type: ToExpr, T8: Type: ToExpr, T9: Type: ToExpr, T10: Type: ToExpr, T11: Type: ToExpr, T12: Type: ToExpr, T13: Type: ToExpr, T14: Type: ToExpr, T15: Type: ToExpr, T16: Type: ToExpr, T17: Type: ToExpr, T18: Type: ToExpr, T19: Type: ToExpr, T20: Type: ToExpr, T21: Type: ToExpr, T22: Type: ToExpr]: ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] with {
    def apply(tup: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22])(using Quotes) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}, ${Expr(x21)}, ${Expr(x22)}) }
    }
  }

  /** Default implementation of `ToExpr[H *: T]`. */
  given TupleConsToExpr [H: Type: ToExpr, T <: Tuple: Type: ToExpr]: ToExpr[H *: T] with {
    def apply(tup: H *: T)(using Quotes): Expr[H *: T] =
      val head = Expr[H](tup.head)
      val tail = Expr[T](tup.tail)
      '{ $head *: $tail }
  }

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
