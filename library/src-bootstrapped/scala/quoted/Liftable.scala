package scala.quoted

import scala.reflect.ClassTag

/** A type class for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'{...}` operation.
 */
trait Liftable[T] {

  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T): QuoteContext ?=> Expr[T]

}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {

  given BooleanIsLiftable[T <: Boolean] as Liftable[T] = new PrimitiveLiftable
  given ByteIsLiftable[T <: Byte] as Liftable[T] = new PrimitiveLiftable
  given ShortIsLiftable[T <: Short] as Liftable[T] = new PrimitiveLiftable
  given IntIsLiftable[T <: Int] as Liftable[T] = new PrimitiveLiftable
  given LongIsLiftable[T <: Long] as Liftable[T] = new PrimitiveLiftable
  given FloatIsLiftable[T <: Float] as Liftable[T] = new PrimitiveLiftable
  given DoubleIsLiftable[T <: Double] as Liftable[T] = new PrimitiveLiftable
  given CharIsLiftable[T <: Char] as Liftable[T] = new PrimitiveLiftable
  given StringIsLiftable[T <: String] as Liftable[T] = new PrimitiveLiftable

  private class PrimitiveLiftable[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends Liftable[T] {
    /** Lift a primitive value `n` into `'{ n }` */
    def toExpr(x: T) = qctx ?=> {
      import qctx.tasty._
      Literal(Constant(x)).seal.asInstanceOf[Expr[T]]
    }
  }

  given ClassIsLiftable[T] as Liftable[Class[T]] = new Liftable[Class[T]] {
    /** Lift a `Class[T]` into `'{ classOf[T] }` */
    def toExpr(x: Class[T]) = qctx ?=> {
      import qctx.tasty._
      Ref(defn.Predef_classOf).appliedToType(Type(x)).seal.asInstanceOf[Expr[Class[T]]]
    }
  }

  given ClassTagIsLiftable[T: Staged] as Liftable[ClassTag[T]] = new Liftable[ClassTag[T]] {
    def toExpr(ct: ClassTag[T]): QuoteContext ?=> Expr[ClassTag[T]] =
      '{ ClassTag[T](${Expr(ct.runtimeClass.asInstanceOf[Class[T]])}) }
  }

  given ArrayIsLiftable[T: Staged: Liftable: ClassTag] as Liftable[Array[T]] = new Liftable[Array[T]] {
    def toExpr(arr: Array[T]): QuoteContext ?=> Expr[Array[T]] =
      '{ Array[T](${Expr(arr.toSeq)}: _*)(${Expr(summon[ClassTag[T]])}) }
  }

  given ArrayOfBooleanIsLiftable as Liftable[Array[Boolean]] = new Liftable[Array[Boolean]] {
    def toExpr(array: Array[Boolean]): QuoteContext ?=> Expr[Array[Boolean]] =
      if (array.length == 0) '{ Array.emptyBooleanArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfByteIsLiftable as Liftable[Array[Byte]] = new Liftable[Array[Byte]] {
    def toExpr(array: Array[Byte]): QuoteContext ?=> Expr[Array[Byte]] =
      if (array.length == 0) '{ Array.emptyByteArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfShortIsLiftable as Liftable[Array[Short]] = new Liftable[Array[Short]] {
    def toExpr(array: Array[Short]): QuoteContext ?=> Expr[Array[Short]] =
      if (array.length == 0) '{ Array.emptyShortArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfCharIsLiftable as Liftable[Array[Char]] = new Liftable[Array[Char]] {
    def toExpr(array: Array[Char]): QuoteContext ?=> Expr[Array[Char]] =
      if (array.length == 0) '{ Array.emptyCharArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfIntIsLiftable as Liftable[Array[Int]] = new Liftable[Array[Int]] {
    def toExpr(array: Array[Int]): QuoteContext ?=> Expr[Array[Int]] =
      if (array.length == 0) '{ Array.emptyIntArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfLongIsLiftable as Liftable[Array[Long]] = new Liftable[Array[Long]] {
    def toExpr(array: Array[Long]): QuoteContext ?=> Expr[Array[Long]] =
      if (array.length == 0) '{ Array.emptyLongArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfFloatIsLiftable as Liftable[Array[Float]] = new Liftable[Array[Float]] {
    def toExpr(array: Array[Float]): QuoteContext ?=> Expr[Array[Float]] =
      if (array.length == 0) '{ Array.emptyFloatArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given ArrayOfDoubleIsLiftable as Liftable[Array[Double]] = new Liftable[Array[Double]] {
    def toExpr(array: Array[Double]): QuoteContext ?=> Expr[Array[Double]] =
      if (array.length == 0) '{ Array.emptyDoubleArray }
      else '{ Array(${Expr(array(0))}, ${Expr(array.toSeq.tail)}: _*) }
  }

  given iArrayIsLiftable[T: Staged](using ltArray: Liftable[Array[T]]) as Liftable[IArray[T]] {
    def toExpr(iarray: IArray[T]): QuoteContext ?=> Expr[IArray[T]] =
      '{ ${ltArray.toExpr(iarray.asInstanceOf[Array[T]])}.asInstanceOf[IArray[T]] }
  }

  given [T: Staged: Liftable] as Liftable[Seq[T]] = new Liftable[Seq[T]] {
    def toExpr(xs: Seq[T]): QuoteContext ?=> Expr[Seq[T]] =
      Expr.ofSeq(xs.map(summon[Liftable[T]].toExpr))
  }

  given [T: Staged: Liftable] as Liftable[List[T]] = new Liftable[List[T]] {
    def toExpr(xs: List[T]): QuoteContext ?=> Expr[List[T]] =
      Expr.ofList(xs.map(summon[Liftable[T]].toExpr))
  }

  given [T: Staged: Liftable] as Liftable[Set[T]] = new Liftable[Set[T]] {
    def toExpr(set: Set[T]): QuoteContext ?=> Expr[Set[T]] =
      '{ Set(${Expr(set.toSeq)}: _*) }
  }

  given [T: Staged: Liftable, U: Staged: Liftable] as Liftable[Map[T, U]] = new Liftable[Map[T, U]] {
    def toExpr(map: Map[T, U]): QuoteContext ?=> Expr[Map[T, U]] =
    '{ Map(${Expr(map.toSeq)}: _*) }
  }

  given [T: Staged: Liftable] as Liftable[Option[T]] = new Liftable[Option[T]] {
    def toExpr(x: Option[T]): QuoteContext ?=> Expr[Option[T]] = x match {
      case Some(x) => '{ Some[T](${Expr(x)}) }
      case None => '{ None: Option[T] }
    }
  }

  given [L: Staged: Liftable, R: Staged: Liftable] as Liftable[Either[L, R]] = new Liftable[Either[L, R]] {
    def toExpr(x: Either[L, R]): QuoteContext ?=> Expr[Either[L, R]] = x match {
      case Left(x) => '{ Left[L, R](${Expr(x)}) }
      case Right(x) => '{ Right[L, R](${Expr(x)}) }
    }
  }

  given [T1: Staged: Liftable] as Liftable[Tuple1[T1]] = new {
    def toExpr(tup: Tuple1[T1]) =
      '{ Tuple1(${Expr(tup._1)}) }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable] as Liftable[Tuple2[T1, T2]] = new {
    def toExpr(tup: Tuple2[T1, T2]) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}) }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable] as Liftable[Tuple3[T1, T2, T3]] = new {
    def toExpr(tup: Tuple3[T1, T2, T3]) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}) }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable] as Liftable[Tuple4[T1, T2, T3, T4]] = new {
    def toExpr(tup: Tuple4[T1, T2, T3, T4]) =
      '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}) }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable] as Liftable[Tuple5[T1, T2, T3, T4, T5]] = new {
    def toExpr(tup: Tuple5[T1, T2, T3, T4, T5]) = {
      val (x1, x2, x3, x4, x5) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable] as Liftable[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def toExpr(tup: Tuple6[T1, T2, T3, T4, T5, T6]) = {
      val (x1, x2, x3, x4, x5, x6) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable] as Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def toExpr(tup: Tuple7[T1, T2, T3, T4, T5, T6, T7]) = {
      val (x1, x2, x3, x4, x5, x6, x7) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable] as Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def toExpr(tup: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable] as Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def toExpr(tup: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable] as Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def toExpr(tup: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable] as Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def toExpr(tup: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable] as Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def toExpr(tup: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable] as Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def toExpr(tup: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable] as Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def toExpr(tup: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable] as Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def toExpr(tup: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable] as Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def toExpr(tup: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable, T17: Staged: Liftable] as Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def toExpr(tup: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable, T17: Staged: Liftable, T18: Staged: Liftable] as Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def toExpr(tup: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable, T17: Staged: Liftable, T18: Staged: Liftable, T19: Staged: Liftable] as Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def toExpr(tup: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable, T17: Staged: Liftable, T18: Staged: Liftable, T19: Staged: Liftable, T20: Staged: Liftable] as Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def toExpr(tup: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable, T17: Staged: Liftable, T18: Staged: Liftable, T19: Staged: Liftable, T20: Staged: Liftable, T21: Staged: Liftable] as Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def toExpr(tup: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}, ${Expr(x21)}) }
    }
  }

  given [T1: Staged: Liftable, T2: Staged: Liftable, T3: Staged: Liftable, T4: Staged: Liftable, T5: Staged: Liftable, T6: Staged: Liftable, T7: Staged: Liftable, T8: Staged: Liftable, T9: Staged: Liftable, T10: Staged: Liftable, T11: Staged: Liftable, T12: Staged: Liftable, T13: Staged: Liftable, T14: Staged: Liftable, T15: Staged: Liftable, T16: Staged: Liftable, T17: Staged: Liftable, T18: Staged: Liftable, T19: Staged: Liftable, T20: Staged: Liftable, T21: Staged: Liftable, T22: Staged: Liftable] as Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def toExpr(tup: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) = tup
      '{ (${Expr(x1)}, ${Expr(x2)}, ${Expr(x3)}, ${Expr(x4)}, ${Expr(x5)}, ${Expr(x6)}, ${Expr(x7)}, ${Expr(x8)}, ${Expr(x9)}, ${Expr(x10)}, ${Expr(x11)}, ${Expr(x12)}, ${Expr(x13)}, ${Expr(x14)}, ${Expr(x15)}, ${Expr(x16)}, ${Expr(x17)}, ${Expr(x18)}, ${Expr(x19)}, ${Expr(x20)}, ${Expr(x21)}, ${Expr(x22)}) }
    }
  }

  given [H: Staged: Liftable, T <: Tuple: Staged: Liftable] as Liftable[H *: T] = new {
    def toExpr(tup: H *: T): QuoteContext ?=> Expr[H *: T] =
      '{ ${summon[Liftable[H]].toExpr(tup.head)} *: ${summon[Liftable[T]].toExpr(tup.tail)} }
      // '{ ${Expr(tup.head)} *: ${Expr(tup.tail)} } // TODO figure out why this fails during CI documentation
  }

  given Liftable[BigInt] = new Liftable[BigInt] {
    def toExpr(x: BigInt): QuoteContext ?=> Expr[BigInt] =
      '{ BigInt(${Expr(x.toByteArray)}) }
  }

  /** Lift a BigDecimal using the default MathContext */
  given Liftable[BigDecimal] = new Liftable[BigDecimal] {
    def toExpr(x: BigDecimal): QuoteContext ?=> Expr[BigDecimal] =
      '{ BigDecimal(${Expr(x.toString)}) }
  }

}
