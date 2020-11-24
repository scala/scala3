package scala.quoted

import scala.reflect.ClassTag

/** A type class for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'{...}` operation.
 */
trait Liftable[T] {

  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T): Quotes ?=> Expr[T]

}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {

  // IMPORTANT Keep in sync with tests/run-staging/liftables.scala

  /** Default liftable for Boolean */
  given BooleanLiftable[T <: Boolean] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Boolean(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Byte */
  given ByteLiftable[T <: Byte] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Byte(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Short */
  given ShortLiftable[T <: Short] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Short(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Int */
  given IntLiftable[T <: Int] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Int(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Long */
  given LongLiftable[T <: Long] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Long(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Float */
  given FloatLiftable[T <: Float] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Float(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Double */
  given DoubleLiftable[T <: Double] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Double(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Char */
  given CharLiftable[T <: Char] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.Char(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for String */
  given StringLiftable[T <: String] as Liftable[T] {
    def toExpr(x: T) =
      import quotes.reflect._
      Literal(Constant.String(x)).asExpr.asInstanceOf[Expr[T]]
  }

  /** Default liftable for Class[T] */
  given ClassLiftable[T] as Liftable[Class[T]] = new Liftable[Class[T]] {
    def toExpr(x: Class[T]) = {
      import quotes.reflect._
      Ref(defn.Predef_classOf).appliedToType(TypeRepr.typeConstructorOf(x)).asExpr.asInstanceOf[Expr[Class[T]]]
    }
  }

  /** Default liftable for ClassTag[T] */
  given ClassTagLiftable[T: Type] as Liftable[ClassTag[T]] = new Liftable[ClassTag[T]] {
    def toExpr(ct: ClassTag[T]): Quotes ?=> Expr[ClassTag[T]] =
      '{ ClassTag[T](${Expr(ct.runtimeClass.asInstanceOf[Class[T]])}) }
  }

  /** Default liftable for Array[T] */
  given ArrayLiftable[T: Type: Liftable: ClassTag] as Liftable[Array[T]] = new Liftable[Array[T]] {
    def toExpr(arr: Array[T]): Quotes ?=> Expr[Array[T]] =
      '{ Array[T](${Expr(arr.toSeq)}: _*)(${Expr(summon[ClassTag[T]])}) }
  }

  /** Default liftable for Array[Boolean] */
  given ArrayOfBooleanLiftable as Liftable[Array[Boolean]] = new Liftable[Array[Boolean]] {
    def toExpr(array: Array[Boolean]): Quotes ?=> Expr[Array[Boolean]] =
      if (array.length == 0) '{ Array.emptyBooleanArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Byte] */
  given ArrayOfByteLiftable as Liftable[Array[Byte]] = new Liftable[Array[Byte]] {
    def toExpr(array: Array[Byte]): Quotes ?=> Expr[Array[Byte]] =
      if (array.length == 0) '{ Array.emptyByteArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Short] */
  given ArrayOfShortLiftable as Liftable[Array[Short]] = new Liftable[Array[Short]] {
    def toExpr(array: Array[Short]): Quotes ?=> Expr[Array[Short]] =
      if (array.length == 0) '{ Array.emptyShortArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Char] */
  given ArrayOfCharLiftable as Liftable[Array[Char]] = new Liftable[Array[Char]] {
    def toExpr(array: Array[Char]): Quotes ?=> Expr[Array[Char]] =
      if (array.length == 0) '{ Array.emptyCharArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Int] */
  given ArrayOfIntLiftable as Liftable[Array[Int]] = new Liftable[Array[Int]] {
    def toExpr(array: Array[Int]): Quotes ?=> Expr[Array[Int]] =
      if (array.length == 0) '{ Array.emptyIntArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Long] */
  given ArrayOfLongLiftable as Liftable[Array[Long]] = new Liftable[Array[Long]] {
    def toExpr(array: Array[Long]): Quotes ?=> Expr[Array[Long]] =
      if (array.length == 0) '{ Array.emptyLongArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Float] */
  given ArrayOfFloatLiftable as Liftable[Array[Float]] = new Liftable[Array[Float]] {
    def toExpr(array: Array[Float]): Quotes ?=> Expr[Array[Float]] =
      if (array.length == 0) '{ Array.emptyFloatArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for Array[Double] */
  given ArrayOfDoubleLiftable as Liftable[Array[Double]] = new Liftable[Array[Double]] {
    def toExpr(array: Array[Double]): Quotes ?=> Expr[Array[Double]] =
      if (array.length == 0) '{ Array.emptyDoubleArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  /** Default liftable for IArray[T] */
  given IArrayLiftable[T: Type](using ltArray: Liftable[Array[T]]) as Liftable[IArray[T]] {
    def toExpr(iarray: IArray[T]): Quotes ?=> Expr[IArray[T]] =
      '{ ${ltArray.toExpr(iarray.asInstanceOf[Array[T]])}.asInstanceOf[IArray[T]] }
  }

  /** Default liftable for Seq[T] */
  given SeqLiftable[T: Type: Liftable] as Liftable[Seq[T]] = new Liftable[Seq[T]] {
    def toExpr(xs: Seq[T]): Quotes ?=> Expr[Seq[T]] =
      Expr.ofSeq(xs.map(summon[Liftable[T]].toExpr))
  }

  /** Default liftable for List[T] */
  given ListLiftable[T: Type: Liftable] as Liftable[List[T]] = new Liftable[List[T]] {
    def toExpr(xs: List[T]): Quotes ?=> Expr[List[T]] =
      Expr.ofList(xs.map(summon[Liftable[T]].toExpr))
  }

  /** Default liftable for Nil.type */
  given NilLiftable as Liftable[Nil.type] = new Liftable[Nil.type] {
    def toExpr(xs: Nil.type): Quotes ?=> Expr[Nil.type] =
      '{ Nil }
  }

  /** Default liftable for Set[T] */
  given SetLiftable[T: Type: Liftable] as Liftable[Set[T]] = new Liftable[Set[T]] {
    def toExpr(set: Set[T]): Quotes ?=> Expr[Set[T]] =
      '{ Set(${Expr(set.toSeq)}: _*) }
  }

  /** Default liftable for Map[T, U] */
  given MapLiftable[T: Type: Liftable, U: Type: Liftable] as Liftable[Map[T, U]] = new Liftable[Map[T, U]] {
    def toExpr(map: Map[T, U]): Quotes ?=> Expr[Map[T, U]] =
    '{ Map(${Expr(map.toSeq)}: _*) }
  }

  /** Default liftable for Option[T] */
  given OptionLiftable[T: Type: Liftable] as Liftable[Option[T]] = new Liftable[Option[T]] {
    def toExpr(x: Option[T]): Quotes ?=> Expr[Option[T]] = x match {
      case x: Some[T] => Expr(x)
      case None => Expr(None)
    }
  }

  /** Default liftable for Some[T] */
  given SomeLiftable[T: Type: Liftable] as Liftable[Some[T]] = new Liftable[Some[T]] {
    def toExpr(x: Some[T]): Quotes ?=> Expr[Some[T]] =
      '{ Some[T](${Expr(x.get)}) }
  }

  /** Default liftable for None.type */
  given NoneLiftable as Liftable[None.type] = new Liftable[None.type] {
    def toExpr(x: None.type): Quotes ?=> Expr[None.type] =
      '{ None }
  }

  /** Default liftable for Either[L, R] */
  given EitherLiftable[L: Type: Liftable, R: Type: Liftable] as Liftable[Either[L, R]] = new Liftable[Either[L, R]] {
    def toExpr(x: Either[L, R]): Quotes ?=> Expr[Either[L, R]] = x match
      case x: Left[L, R] => Expr(x)
      case x: Right[L, R] => Expr(x)
  }

  /** Default liftable for Left[L, R] */
  given LeftLiftable[L: Type: Liftable, R: Type] as Liftable[Left[L, R]] = new Liftable[Left[L, R]] {
    def toExpr(x: Left[L, R]): Quotes ?=> Expr[Left[L, R]] =
      '{ Left[L, R](${Expr(x.value)}) }
  }

  /** Default liftable for Right[L, R] */
  given RightLiftable[L: Type, R: Type: Liftable] as Liftable[Right[L, R]] = new Liftable[Right[L, R]] {
    def toExpr(x: Right[L, R]): Quotes ?=> Expr[Right[L, R]] =
      '{ Right[L, R](${Expr(x.value)}) }
  }

  /** Default liftable for EmptyTuple.type */
  given EmptyTupleLiftable as Liftable[EmptyTuple.type] = new {
    def toExpr(tup: EmptyTuple.type) =
      '{ EmptyTuple }
  }

  /** Default liftable for Tuple1[T1] */
  given Tuple1Liftable[T1: Type: Liftable] as Liftable[Tuple1[T1]] = new {
    def toExpr(tup: Tuple1[T1]) =
      '{ Tuple1(${Expr(tup._1)}) }
  }

  /** Default liftable for Tuple2[T1, T2] */
  given Tuple2Liftable[T1: Type: Liftable, T2: Type: Liftable] as Liftable[Tuple2[T1, T2]] = new {
    def toExpr(tup: Tuple2[T1, T2]) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}) }
  }

  /** Default liftable for Tuple3[T1, T2, T3] */
  given Tuple3Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable] as Liftable[Tuple3[T1, T2, T3]] = new {
    def toExpr(tup: Tuple3[T1, T2, T3]) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}) }
  }

  /** Default liftable for Tuple4[T1, T2, T3, T4] */
  given Tuple4Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable] as Liftable[Tuple4[T1, T2, T3, T4]] = new {
    def toExpr(tup: Tuple4[T1, T2, T3, T4]) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}) }
  }

  /** Default liftable for Tuple5[T1, T2, T3, T4, T5] */
  given Tuple5Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable] as Liftable[Tuple5[T1, T2, T3, T4, T5]] = new {
    def toExpr(tup: Tuple5[T1, T2, T3, T4, T5]) = {
      val (x1, x2, x3, x4, x5) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}) }
    }
  }

  /** Default liftable for Tuple6[T1, T2, T3, T4, T5, T6] */
  given Tuple6Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable] as Liftable[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def toExpr(tup: Tuple6[T1, T2, T3, T4, T5, T6]) = {
      val (x1, x2, x3, x4, x5, x6) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}) }
    }
  }

  /** Default liftable for Tuple7[T1, T2, T3, T4, T5, T6, T7] */
  given Tuple7Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable] as Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def toExpr(tup: Tuple7[T1, T2, T3, T4, T5, T6, T7]) = {
      val (x1, x2, x3, x4, x5, x6, x7) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}) }
    }
  }

  /** Default liftable for Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] */
  given Tuple8Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable] as Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def toExpr(tup: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}) }
    }
  }

  /** Default liftable for Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] */
  given Tuple9Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable] as Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def toExpr(tup: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}) }
    }
  }

  /** Default liftable for Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] */
  given Tuple10Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable] as Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def toExpr(tup: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}) }
    }
  }

  /** Default liftable for Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] */
  given Tuple11Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable] as Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def toExpr(tup: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}) }
    }
  }

  /** Default liftable for Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] */
  given Tuple12Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable] as Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def toExpr(tup: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}) }
    }
  }

  /** Default liftable for Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] */
  given Tuple13Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable] as Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def toExpr(tup: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}) }
    }
  }

  /** Default liftable for Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] */
  given Tuple14Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable] as Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def toExpr(tup: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}) }
    }
  }

  /** Default liftable for Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] */
  given Tuple15Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable] as Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def toExpr(tup: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}) }
    }
  }

  /** Default liftable for Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] */
  given Tuple16Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable] as Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def toExpr(tup: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}) }
    }
  }

  /** Default liftable for Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] */
  given Tuple17Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable] as Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def toExpr(tup: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}) }
    }
  }

  /** Default liftable for Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] */
  given Tuple18Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable] as Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def toExpr(tup: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}) }
    }
  }

  /** Default liftable for Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] */
  given Tuple19Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable] as Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def toExpr(tup: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}) }
    }
  }

  /** Default liftable for Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] */
  given Tuple20Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable, T20: Type: Liftable] as Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def toExpr(tup: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}) }
    }
  }

  /** Default liftable for Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] */
  given Tuple21Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable, T20: Type: Liftable, T21: Type: Liftable] as Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def toExpr(tup: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}, ${Expr(x21)}) }
    }
  }

  /** Default liftable for Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] */
  given Tuple22Liftable[T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable, T20: Type: Liftable, T21: Type: Liftable, T22: Type: Liftable] as Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def toExpr(tup: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}, ${Expr(x21)}, ${Expr(x22)}) }
    }
  }

  /** Default liftable for H *: T */
  given TupleConsLiftable [H: Type: Liftable, T <: Tuple: Type: Liftable] as Liftable[H *: T] = new {
    def toExpr(tup: H *: T): Quotes ?=> Expr[H *: T] =
      '{ ${summon[Liftable[H]].toExpr(tup.head)} *: ${summon[Liftable[T]].toExpr(tup.tail)} }
      // '{ ${Expr(tup.head)} *: ${Expr(tup.tail)} } // TODO figure out why this fails during CI documentation
  }

  /** Default liftable for BigInt */
  given BigIntLiftable as Liftable[BigInt] = new Liftable[BigInt] {
    def toExpr(x: BigInt): Quotes ?=> Expr[BigInt] =
      '{ BigInt(${Expr(x.toByteArray)}) }
  }

  /** Default liftable for BigDecimal using the default MathContext */
  given BigDecimalLiftable as Liftable[BigDecimal] = new Liftable[BigDecimal] {
    def toExpr(x: BigDecimal): Quotes ?=> Expr[BigDecimal] =
      '{ BigDecimal(${Expr(x.toString)}) }
  }

  /** Default liftable for StringContext */
  given StringContextLiftable as Liftable[StringContext] = new Liftable[StringContext] {
    def toExpr(stringContext: StringContext): Quotes ?=> Expr[StringContext] =
      val parts = Varargs(stringContext.parts.map(Expr(_)))
      '{ StringContext($parts: _*) }
  }

}
