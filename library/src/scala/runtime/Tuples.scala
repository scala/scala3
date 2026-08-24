package scala.runtime

import language.experimental.captureChecking

object Tuples {

  /** The largest arity for which tuples have a dedicated `TupleN` class (22); larger tuples are represented by a [[TupleXXL]] backed by an array. */
  inline val MaxSpecialized = 22

  /** Returns the elements of `self` in an `Array[Object]`; implements [[scala.Tuple.toArray]].
   *
   *  For `EmptyTuple` the shared empty array is returned; otherwise the array
   *  is freshly allocated (a [[TupleXXL]]'s backing array is cloned), so
   *  mutating it does not affect the tuple.
   *
   *  @param self the tuple whose elements are extracted
   *  @return an array containing the elements of `self`, in order
   */
  def toArray(self: Tuple): Array[Object] = (self: Any) match {
    case EmptyTuple => Array.emptyObjectArray
    case self: TupleXXL => self.toArray
    case self: Product => productToArray(self)
  }

  /** Returns the elements of `self` in an `IArray[Object]`; implements [[scala.Tuple.toIArray]].
   *
   *  For a [[TupleXXL]] this is the tuple's backing array itself, not a copy;
   *  for `EmptyTuple` it is the shared empty array; for `Tuple1` to `Tuple22`
   *  a fresh array is filled from the product elements.
   *
   *  @param self the tuple whose elements are extracted
   *  @return an immutable array containing the elements of `self`, in order
   */
  def toIArray(self: Tuple): IArray[Object] = (self: Any) match {
    case EmptyTuple => Array.emptyObjectArray.asInstanceOf[IArray[Object]]
    case self: TupleXXL => self.elems
    case self: Product => productToArray(self).asInstanceOf[IArray[Object]]
  }

  /** Copies the elements of `self` into a fresh `Array[Object]`.
   *
   *  Accepts any `Product`, not just tuples. [[toArray]] uses it for `Tuple1`
   *  to `Tuple22`, and the compiler calls it directly when optimizing
   *  `Tuple.toArray` on tuples statically known to have at most 22 elements.
   *
   *  @param self the product whose elements are copied
   *  @return a fresh array containing `self.productElement(0)` to `self.productElement(self.productArity - 1)`
   */
  def productToArray(self: Product): Array[Object] = {
    val arr = new Array[Object](self.productArity)
    var i = 0
    while (i < arr.length) {
      arr(i) = self.productElement(i).asInstanceOf[Object]
      i += 1
    }
    arr
  }

  /** Returns a tuple containing the elements of `xs`, in order; backs [[scala.Tuple.fromArray]].
   *
   *  Arrays of length 0 to 22 yield `EmptyTuple` or the corresponding `TupleN`;
   *  longer arrays yield a [[TupleXXL]] wrapping a clone of `xs`. In either
   *  case, mutating `xs` afterwards does not affect the result. The elements
   *  must already be boxed; [[scala.Tuple.fromArray]] takes care of that.
   *
   *  @param xs the array supplying the elements of the tuple
   *  @return a tuple of arity `xs.length` with the elements of `xs`
   */
  def fromArray(xs: Array[Object]): Tuple = xs.length match {
    case 0  => EmptyTuple
    case 1  => Tuple1(xs(0))
    case 2  => Tuple2(xs(0), xs(1))
    case 3  => Tuple3(xs(0), xs(1), xs(2))
    case 4  => Tuple4(xs(0), xs(1), xs(2), xs(3))
    case 5  => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4))
    case 6  => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5))
    case 7  => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6))
    case 8  => Tuple8(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7))
    case 9  => Tuple9(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8))
    case 10 => Tuple10(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9))
    case 11 => Tuple11(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10))
    case 12 => Tuple12(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11))
    case 13 => Tuple13(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12))
    case 14 => Tuple14(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13))
    case 15 => Tuple15(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14))
    case 16 => Tuple16(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15))
    case 17 => Tuple17(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16))
    case 18 => Tuple18(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17))
    case 19 => Tuple19(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18))
    case 20 => Tuple20(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19))
    case 21 => Tuple21(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20))
    case 22 => Tuple22(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20), xs(21))
    case _ => TupleXXL.fromIArray(xs.clone().asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
  }

  /** Returns a tuple containing the elements of `xs`, in order; backs [[scala.Tuple.fromIArray]].
   *
   *  Arrays of length 0 to 22 yield `EmptyTuple` or the corresponding `TupleN`,
   *  as in [[fromArray]]; longer arrays yield a [[TupleXXL]] backed directly by
   *  `xs`, without copying.
   *
   *  @param xs the immutable array supplying the elements of the tuple
   *  @return a tuple of arity `xs.length` with the elements of `xs`
   */
  def fromIArray(xs: IArray[Object]): Tuple =
    if (xs.length <= 22) fromArray(xs.asInstanceOf[Array[Object]])
    else TupleXXL.fromIArray(xs).asInstanceOf[Tuple]

  /** Returns a tuple containing the elements of `xs`, in order; backs [[scala.Tuple.fromProduct]] and [[TupleMirror.fromProduct]].
   *
   *  If `xs` already is a tuple in the runtime representation matching its
   *  arity (a `TupleN` for arities 1 to 22, a [[TupleXXL]] beyond), it is
   *  returned as is; otherwise its elements are copied into a new tuple of
   *  arity `xs.productArity`. Every product of arity 0 yields `EmptyTuple`.
   *
   *  @param xs the product supplying the elements of the tuple
   *  @return a tuple of arity `xs.productArity` with the elements of `xs`
   */
  def fromProduct(xs: Product): Tuple = (xs.productArity match {
    case 0  => EmptyTuple
    case 1 =>
      xs match {
        case xs: Tuple1[?] => xs
        case xs => Tuple1(xs.productElement(0))
      }
    case 2 =>
      xs match {
        case xs: Tuple2[?, ?] => xs
        case xs => Tuple2(xs.productElement(0), xs.productElement(1))
      }
    case 3 =>
      xs match {
        case xs: Tuple3[?, ?, ?] => xs
        case xs => Tuple3(xs.productElement(0), xs.productElement(1), xs.productElement(2))
      }
    case 4 =>
      xs match {
        case xs: Tuple4[?, ?, ?, ?] => xs
        case xs => Tuple4(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3))
      }
    case 5 =>
      xs match {
        case xs: Tuple5[?, ?, ?, ?, ?] => xs
        case xs => Tuple5(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4))
      }
    case 6 =>
      xs match {
        case xs: Tuple6[?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple6(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5))
      }
    case 7 =>
      xs match {
        case xs: Tuple7[?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple7(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6))
      }
    case 8 =>
      xs match {
        case xs: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple8(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7))
      }
    case 9 =>
      xs match {
        case xs: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple9(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8))
      }
    case 10 =>
      xs match {
        case xs: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple10(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9))
      }
    case 11 =>
      xs match {
        case xs: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple11(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10))
      }
    case 12 =>
      xs match {
        case xs: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple12(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11))
      }
    case 13 =>
      xs match {
        case xs: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple13(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12))
      }
    case 14 =>
      xs match {
        case xs: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple14(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13))
      }
    case 15 =>
      xs match {
        case xs: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple15(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14))
      }
    case 16 =>
      xs match {
        case xs: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple16(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15))
      }
    case 17 =>
      xs match {
        case xs: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple17(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16))
      }
    case 18 =>
      xs match {
        case xs: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple18(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17))
      }
    case 19 =>
      xs match {
        case xs: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple19(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18))
      }
    case 20 =>
      xs match {
        case xs: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple20(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19))
      }
    case 21 =>
      xs match {
        case xs: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple21(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20))
      }
    case 22 =>
      xs match {
        case xs: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple22(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20), xs.productElement(21))
      }
    case _ =>
      (xs match {
        case xs: TupleXXL => xs
        case xs => TupleXXL.fromIArray(xs.productIterator.map(_.asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use Iterator.toIArray
      }).asInstanceOf[Tuple]
  })

  // Cons for Tuple1 to Tuple22
  private def specialCaseCons(x: Any, self: Tuple): Tuple = {
    (self: Any) match {
      case EmptyTuple =>
        Tuple1(x)
      case self: Tuple1[?] =>
        Tuple2(x, self._1)
      case self: Tuple2[?, ?] =>
        Tuple3(x, self._1, self._2)
      case self: Tuple3[?, ?, ?] =>
        Tuple4(x, self._1, self._2, self._3)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple5(x, self._1, self._2, self._3, self._4)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple6(x, self._1, self._2, self._3, self._4, self._5)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple7(x, self._1, self._2, self._3, self._4, self._5, self._6)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple22(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        val arr: Array[Object] = Array(
          x.asInstanceOf[Object], self._1.asInstanceOf[Object], self._2.asInstanceOf[Object],
          self._3.asInstanceOf[Object], self._4.asInstanceOf[Object], self._5.asInstanceOf[Object],
          self._6.asInstanceOf[Object], self._7.asInstanceOf[Object], self._8.asInstanceOf[Object],
          self._9.asInstanceOf[Object], self._10.asInstanceOf[Object], self._11.asInstanceOf[Object],
          self._12.asInstanceOf[Object], self._13.asInstanceOf[Object], self._14.asInstanceOf[Object],
          self._15.asInstanceOf[Object], self._16.asInstanceOf[Object], self._17.asInstanceOf[Object],
          self._18.asInstanceOf[Object], self._19.asInstanceOf[Object], self._20.asInstanceOf[Object],
          self._21.asInstanceOf[Object], self._22.asInstanceOf[Object],
        )
        TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  // Cons for TupleXXL
  private def xxlCons(x: Any, xxl: TupleXXL): TupleXXL = {
    val arr = new Array[Object](xxl.productArity + 1)
    arr(0) = x.asInstanceOf[Object]
    System.arraycopy(xxl.elems, 0, arr, 1, xxl.productArity)
    TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]])
  }

  /** Returns a new tuple with `x` prepended to the elements of `self`; implements [[scala.Tuple.*:]].
   *
   *  The result has arity `self.size + 1`: a `Tuple22` or [[TupleXXL]] receiver
   *  yields a `TupleXXL`, smaller receivers yield the next larger `TupleN`.
   *
   *  @param x the element to prepend
   *  @param self the tuple supplying the remaining elements
   *  @return a tuple with `x` as its 1st element, followed by the elements of `self`
   */
  def cons(x: Any, self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlCons(x, xxl).asInstanceOf[Tuple]
    case _ => specialCaseCons(x, self)
  }

  /** Returns the concatenation of `self` and `that`; implements [[scala.Tuple.++]].
   *
   *  If either tuple is empty, the other tuple is returned unchanged (the same
   *  instance, not a copy); otherwise the elements of both are copied into a
   *  fresh tuple of arity `self.size + that.size`.
   *
   *  @tparam This the type of the first tuple
   *  @tparam That the type of the second tuple
   *  @param self the tuple supplying the leading elements
   *  @param that the tuple supplying the trailing elements
   *  @return a tuple with the elements of `self` followed by the elements of `that`
   */
  def concat[This <: Tuple, That <: Tuple](self: This, that: That): Tuple = {
    val selfSize: Int = self.size
    // If one of the tuples is empty, we can leave early
    if selfSize == 0 then
      return that

    val thatSize: Int = that.size
    if thatSize == 0 then
      return self

    val arr = new Array[Object](selfSize + thatSize)

    // Copies the tuple to an array, at the given offset
    inline def copyToArray[T <: Tuple](tuple: T, size: Int, array: Array[Object], offset: Int): Unit = (tuple: Any) match {
      case xxl: TupleXXL =>
        System.arraycopy(xxl.elems, 0, array, offset, size)
      case _ =>
        tuple.productIterator.asInstanceOf[Iterator[Object]]
          .copyToArray(array, offset, size)
    }

    // In the general case, we copy the two tuples to an array, and convert it back to a tuple
    copyToArray(self, selfSize, arr, 0)
    copyToArray(that, thatSize, arr, selfSize)
    fromIArray(arr.asInstanceOf[IArray[Object]])
  }

  /** Returns the number of elements of `self`: 0 for `EmptyTuple`, otherwise its `productArity`; implements [[scala.Tuple.size]].
   *
   *  @param self the tuple whose arity is computed
   *  @return the arity of `self`
   */
  def size(self: Tuple): Int = (self: Any) match {
    case EmptyTuple => 0
    case self: Product => self.productArity
  }

  // Tail for Tuple1 to Tuple22
  private def specialCaseTail(self: Tuple): Tuple = {
    (self: Any) match {
      case self: Tuple1[?] =>
        EmptyTuple
      case self: Tuple2[?, ?] =>
        Tuple1(self._2)
      case self: Tuple3[?, ?, ?] =>
        Tuple2(self._2, self._3)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple3(self._2, self._3, self._4)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple4(self._2, self._3, self._4, self._5)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple5(self._2, self._3, self._4, self._5, self._6)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple6(self._2, self._3, self._4, self._5, self._6, self._7)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple7(self._2, self._3, self._4, self._5, self._6, self._7, self._8)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21, self._22)
    }
  }

  // Tail for TupleXXL
  private def xxlTail(xxl: TupleXXL): Tuple = {
    if (xxl.productArity == 23) {
      val elems = xxl.elems
      Tuple22(
        elems(1), elems(2), elems(3), elems(4), elems(5), elems(6), elems(7),
        elems(8), elems(9), elems(10), elems(11), elems(12), elems(13), elems(14),
        elems(15), elems(16), elems(17), elems(18), elems(19), elems(20),
        elems(21), elems(22)
      )
    } else {
      val arr = new Array[Object](xxl.elems.length - 1)
      System.arraycopy(xxl.elems, 1, arr, 0, xxl.elems.length - 1)
      TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  /** Returns a tuple containing all elements of `self` except the 1st; implements [[scala.Tuple.tail]].
   *
   *  `self` must be non-empty.
   *
   *  @param self the tuple whose tail is taken
   *  @return a tuple of arity `self.size - 1` with all elements of `self` except the 1st
   *  @throws MatchError if `self` is `EmptyTuple`
   */
  def tail(self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlTail(xxl)
    case _ => specialCaseTail(self)
  }

  // Append for TupleXXL
  private def xxlAppend(x: Any, xxl: TupleXXL): TupleXXL = {
    val arr = new Array[Object](xxl.productArity + 1)
    arr(xxl.productArity) = x.asInstanceOf[Object]
    System.arraycopy(xxl.elems, 0, arr, 0, xxl.productArity)
    TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]])
  }

  // Append for Tuple1 to Tuple22
  private def specialCaseAppend(x: Any, self: Tuple): Tuple = {
    (self: Any) match {
      case EmptyTuple =>
        Tuple1(x)
      case self: Tuple1[?] =>
        Tuple2(self._1, x)
      case self: Tuple2[?, ?] =>
        Tuple3(self._1, self._2, x)
      case self: Tuple3[?, ?, ?] =>
        Tuple4(self._1, self._2, self._3, x)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple5(self._1, self._2, self._3, self._4, x)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple6(self._1, self._2, self._3, self._4, self._5, x)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple7(self._1, self._2, self._3, self._4, self._5, self._6, x)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._1, self._2, self._3, self._4, self._5, self._6, self._7, x)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, x)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, x)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, x)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, x)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, x)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, x)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, x)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, x)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, x)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, x)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, x)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, x)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, x)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple22(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21, x)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        val arr: Array[Object] = Array(
          self._1.asInstanceOf[Object], self._2.asInstanceOf[Object],
          self._3.asInstanceOf[Object], self._4.asInstanceOf[Object], self._5.asInstanceOf[Object],
          self._6.asInstanceOf[Object], self._7.asInstanceOf[Object], self._8.asInstanceOf[Object],
          self._9.asInstanceOf[Object], self._10.asInstanceOf[Object], self._11.asInstanceOf[Object],
          self._12.asInstanceOf[Object], self._13.asInstanceOf[Object], self._14.asInstanceOf[Object],
          self._15.asInstanceOf[Object], self._16.asInstanceOf[Object], self._17.asInstanceOf[Object],
          self._18.asInstanceOf[Object], self._19.asInstanceOf[Object], self._20.asInstanceOf[Object],
          self._21.asInstanceOf[Object], self._22.asInstanceOf[Object], x.asInstanceOf[Object]
        )
        TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  /** Returns a new tuple with `x` appended to the elements of `self`; implements [[scala.Tuple.:*]].
   *
   *  The result has arity `self.size + 1`: a `Tuple22` or [[TupleXXL]] receiver
   *  yields a `TupleXXL`, smaller receivers yield the next larger `TupleN`.
   *
   *  @param x the element to append
   *  @param self the tuple supplying the leading elements
   *  @return a tuple with the elements of `self` followed by `x` as its last element
   */
  def append(x: Any, self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlAppend(x, xxl).asInstanceOf[Tuple]
    case _ => specialCaseAppend(x, self)
  }

  // Init for TupleXXL
  private def xxlInit(xxl: TupleXXL): Tuple = {
    if (xxl.productArity == 23) {
      val elems = xxl.elems
      Tuple22(
        elems(0), elems(1), elems(2), elems(3), elems(4), elems(5),
        elems(6), elems(7), elems(8), elems(9), elems(10), elems(11),
        elems(12), elems(13), elems(14), elems(15), elems(16), elems(17),
        elems(18), elems(19), elems(20), elems(21)
      )
    } else {
      val arr = new Array[Object](xxl.elems.length - 1)
      System.arraycopy(xxl.elems, 0, arr, 0, xxl.elems.length - 1)
      TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  // Reverse for TupleXXL
  private def xxlReverse(xxl: TupleXXL): Tuple =
    TupleXXL.fromIArray(xxl.elems.reverse.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]

  // Reverse for Tuple0 to Tuple22
  private def specialCaseReverse(self: Tuple): Tuple = {
    (self: Any) match {
      case EmptyTuple =>
        EmptyTuple
      case self: Tuple1[?] =>
        self
      case self: Tuple2[?, ?] =>
        Tuple2(self._2, self._1)
      case self: Tuple3[?, ?, ?] =>
        Tuple3(self._3, self._2, self._1)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple4(self._4, self._3, self._2, self._1)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple5(self._5, self._4, self._3, self._2, self._1)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple6(self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple7(self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._20, self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._21, self._20, self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple22(self._22, self._21, self._20, self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
    }
  }

  /** Returns a tuple with the elements of `self` in reverse order; implements [[scala.Tuple.reverse]].
   *
   *  `EmptyTuple` and `Tuple1` receivers are returned unchanged (the same
   *  instance); all other arities yield a new tuple of the same arity.
   *
   *  @param self the tuple to reverse
   *  @return a tuple whose i-th element is the i-th element of `self` counted from the end
   */
  def reverse(self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlReverse(xxl)
    case _ => specialCaseReverse(self)
  }

  // Init for Tuple1 to Tuple22
  private def specialCaseInit(self: Tuple): Tuple = {
    (self: Any) match {
      case _: Tuple1[?] =>
        EmptyTuple
      case self: Tuple2[?, ?] =>
        Tuple1(self._1)
      case self: Tuple3[?, ?, ?] =>
        Tuple2(self._1, self._2)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple3(self._1, self._2, self._3)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple4(self._1,self._2, self._3, self._4)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple5(self._1, self._2, self._3, self._4, self._5)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple6(self._1, self._2, self._3, self._4, self._5, self._6)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple7(self._1, self._2, self._3, self._4, self._5, self._6, self._7)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21)
    }
  }

  /** Returns a tuple containing all elements of `self` except the last; implements [[scala.Tuple.init]].
   *
   *  `self` must be non-empty.
   *
   *  @param self the tuple whose initial part is taken
   *  @return a tuple of arity `self.size - 1` with all elements of `self` except the last
   *  @throws MatchError if `self` is `EmptyTuple`
   */
  def init(self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlInit(xxl)
    case _ => specialCaseInit(self)
  }

  /** Returns the last element of `self`; implements [[scala.Tuple.last]].
   *
   *  @param self the tuple whose last element is retrieved
   *  @return the element of `self` at index `self.size - 1`
   *  @throws IndexOutOfBoundsException if `self` is `EmptyTuple` (the element at index -1 is requested)
   */
  def last(self: Tuple): Any = (self: Any) match {
    case self: Product => self.productElement(self.productArity - 1)
  }

  /** Returns the element of `self` at index `n`, via `productElement`; implements [[scala.Tuple.apply]] and [[scala.Tuple.head]].
   *
   *  @param self the tuple whose element is retrieved
   *  @param n the 0-based index of the element
   *  @return the element at index `n`
   *  @throws IndexOutOfBoundsException if `n` is negative or not less than `self.size`
   */
  def apply(self: Tuple, n: Int): Any =
    self.productElement(n)

  // Benchmarks showed that this is faster than doing (it1 zip it2).copyToArray(...)
  private def zipIterators(it1: Iterator[Any], it2: Iterator[Any], size: Int): IArray[Object] = {
    val arr = new Array[Object](size)
    var i = 0
    while (i < size) {
      arr(i) = (it1.next(), it2.next())
      i += 1
    }
    arr.asInstanceOf[IArray[Object]]
  }

  /** Returns a tuple of pairs formed from corresponding elements of `t1` and `t2`; implements [[scala.Tuple.zip]].
   *
   *  The result's arity is the smaller of the two arities; extra elements of
   *  the longer tuple are discarded. If either tuple is empty, the result is
   *  `EmptyTuple`.
   *
   *  @param t1 the tuple supplying the 1st element of each pair
   *  @param t2 the tuple supplying the 2nd element of each pair
   *  @return a tuple whose i-th element is the `Tuple2` of the i-th elements of `t1` and `t2`
   */
  def zip(t1: Tuple, t2: Tuple): Tuple = {
    val t1Size: Int = t1.size
    val t2Size: Int = t2.size
    val size = Math.min(t1Size, t2Size)
    if size == 0 then EmptyTuple
    else Tuple.fromIArray(
      zipIterators(
        t1.productIterator,
        t2.productIterator,
        size
      )
    )
  }

  /** Returns a tuple of the same arity as `self` whose elements are the results of applying `f` to the elements of `self`; implements [[scala.Tuple.map]].
   *
   *  An `EmptyTuple` receiver is returned unchanged, without calling `f`.
   *
   *  @tparam F the type constructor mapping each element type to its result type
   *  @param self the tuple whose elements are transformed
   *  @param f the polymorphic function applied to each element
   *  @return a tuple whose i-th element is `f` applied to the i-th element of `self`
   */
  def map[F[_]](self: Tuple, f: [t] -> t -> F[t]): Tuple = self match {
    case EmptyTuple => self
    case _ => fromIArray(self.productIterator.map(f(_).asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use toIArray
  }

  /** Returns a tuple containing the first `n` elements of `self`; implements [[scala.Tuple.take]].
   *
   *  If `n` is greater than `self.size`, all elements are taken; if `n` is 0
   *  or `self` is empty, the result is `EmptyTuple`.
   *
   *  @param self the tuple whose leading elements are taken
   *  @param n the number of elements to take
   *  @return a tuple with the first `min(n, self.size)` elements of `self`
   *  @throws IndexOutOfBoundsException if `n` is negative
   */
  def take(self: Tuple, n: Int): Tuple = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val selfSize: Int = self.size
    val actualN = Math.min(n, selfSize)

    if (actualN == 0) EmptyTuple
    else {
      val arr = (self: Any) match {
        case xxl: TupleXXL =>
          xxl.elems.asInstanceOf[Array[Object]].take(actualN)
        case _ =>
          val arr = new Array[Object](actualN)
          self.productIterator.asInstanceOf[Iterator[Object]]
            .copyToArray(arr, 0, actualN)
          arr
      }

      fromIArray(arr.asInstanceOf[IArray[Object]])
    }
  }

  /** Returns a tuple containing all elements of `self` except the first `n`; implements [[scala.Tuple.drop]].
   *
   *  If `n` is greater than or equal to `self.size`, the result is `EmptyTuple`;
   *  if `n` is 0, the result is a tuple with all elements of `self`.
   *
   *  @param self the tuple whose leading elements are dropped
   *  @param n the number of elements to drop
   *  @return a tuple with the last `self.size - min(n, self.size)` elements of `self`
   *  @throws IndexOutOfBoundsException if `n` is negative
   */
  def drop(self: Tuple, n: Int): Tuple = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val size = self.size
    val actualN = Math.min(n, size)
    val rem = size - actualN

    if (rem == 0) EmptyTuple
    else {
      val arr = (self: Any) match {
        case xxl: TupleXXL =>
          xxl.elems.asInstanceOf[Array[Object]].drop(actualN)
        case _ =>
          val arr = new Array[Object](rem)
          self.productIterator.asInstanceOf[Iterator[Object]]
            .drop(actualN).copyToArray(arr, 0, rem)
          arr
      }

      fromIArray(arr.asInstanceOf[IArray[Object]])
    }
  }

  /** Splits `self` into two tuples at index `n`; implements [[scala.Tuple.splitAt]].
   *
   *  `n` is clamped to `self.size`, so for larger `n` the second tuple is
   *  `EmptyTuple` and the first contains all elements.
   *
   *  @param self the tuple to split
   *  @param n the number of elements in the first part
   *  @return a pair of the tuple of the first `min(n, self.size)` elements of `self` and the tuple of the remaining elements
   *  @throws IndexOutOfBoundsException if `n` is negative
   */
  def splitAt(self: Tuple, n: Int): (Tuple, Tuple) = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val size = self.size
    val actualN = Math.min(n, size)
    val (arr1, arr2) = (self: Any) match {
      case EmptyTuple => (Array.empty[Object], Array.empty[Object])
      case xxl: TupleXXL =>
        xxl.elems.asInstanceOf[Array[Object]].splitAt(actualN)
      case _ =>
        val arr1 = new Array[Object](actualN)
        val arr2 = new Array[Object](size - actualN)
        val it = self.productIterator.asInstanceOf[Iterator[Object]]
        it.copyToArray(arr1, 0, actualN)
        it.copyToArray(arr2, 0, size - actualN)
        (arr1, arr2)
    }

    (
      fromIArray(arr1.asInstanceOf[IArray[Object]]),
      fromIArray(arr2.asInstanceOf[IArray[Object]])
    )
  }

  /** Returns an iterator over `head` followed by the elements of `tail`.
   *
   *  The compiler emits calls to this method when optimizing `*:` on tuples of
   *  statically known arity, so the elements of `head *: tail` can be consumed
   *  without materializing an intermediate tuple.
   *
   *  @param head the value produced first
   *  @param tail the tuple whose elements are produced after `head`
   *  @return an iterator producing `head`, then the elements of `tail` in order
   */
  def consIterator(head: Any, tail: Tuple): Iterator[Any] =
    Iterator.single(head) ++ tail.productIterator

  /** Returns an iterator over the elements of `tup1` followed by the elements of `tup2`.
   *
   *  The compiler emits calls to this method when optimizing `++` on tuples of
   *  statically known arity, so the elements of `tup1 ++ tup2` can be consumed
   *  without materializing an intermediate tuple.
   *
   *  @param tup1 the tuple whose elements are produced first
   *  @param tup2 the tuple whose elements are produced after those of `tup1`
   *  @return an iterator producing the elements of `tup1`, then those of `tup2`, in order
   */
  def concatIterator(tup1: Tuple, tup2: Tuple): Iterator[Any] =
    tup1.productIterator ++ tup2.productIterator

  /** Returns whether `x` is a tuple at runtime; the compiler rewrites the type test `x.isInstanceOf[Tuple]` to a call of this method.
   *
   *  A value passes the test if it is `EmptyTuple`, an instance of the
   *  `TupleN` class matching its arity, or a [[TupleXXL]]. Other products are
   *  rejected: for example, a case class of arity 2 that is not a `Tuple2` is
   *  not a tuple, and a product of arity 0 is a tuple only if it equals
   *  `EmptyTuple`.
   *
   *  @param x the value to test
   *  @return `true` if `x` is a tuple, `false` otherwise
   */
  def isInstanceOfTuple(x: Any): Boolean =
    x match
      case x: Product =>
        x.productArity match
          case 0 => x == EmptyTuple
          case 1 => x.isInstanceOf[Tuple1[?]]
          case 2 => x.isInstanceOf[Tuple2[?, ?]]
          case 3 => x.isInstanceOf[Tuple3[?, ?, ?]]
          case 4 => x.isInstanceOf[Tuple4[?, ?, ?, ?]]
          case 5 => x.isInstanceOf[Tuple5[?, ?, ?, ?, ?]]
          case 6 => x.isInstanceOf[Tuple6[?, ?, ?, ?, ?, ?]]
          case 7 => x.isInstanceOf[Tuple7[?, ?, ?, ?, ?, ?, ?]]
          case 8 => x.isInstanceOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?]]
          case 9 => x.isInstanceOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 10 => x.isInstanceOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 11 => x.isInstanceOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 12 => x.isInstanceOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 13 => x.isInstanceOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 14 => x.isInstanceOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 15 => x.isInstanceOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 16 => x.isInstanceOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 17 => x.isInstanceOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 18 => x.isInstanceOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 19 => x.isInstanceOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 20 => x.isInstanceOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 21 => x.isInstanceOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 22 => x.isInstanceOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case _ => x.isInstanceOf[TupleXXL]
      case _ =>
        false

  /** Returns whether `x` is the empty tuple; the compiler rewrites the type test `x.isInstanceOf[EmptyTuple]` to a call of this method.
   *
   *  @param x the value to test
   *  @return `true` if `x` equals the `EmptyTuple` singleton, `false` otherwise
   */
  def isInstanceOfEmptyTuple(x: Any): Boolean = x == EmptyTuple

  /** Returns whether `x` is a tuple with at least one element; the compiler rewrites the type tests `x.isInstanceOf[NonEmptyTuple]` and `x.isInstanceOf[_ *: _]` to calls of this method.
   *
   *  A value passes the test if it is an instance of the `TupleN` class
   *  matching its arity, or a [[TupleXXL]]. `EmptyTuple` and products that are
   *  not tuple classes are rejected.
   *
   *  @param x the value to test
   *  @return `true` if `x` is a non-empty tuple, `false` otherwise
   */
  def isInstanceOfNonEmptyTuple(x: Any): Boolean =
    x match
      case x: Product =>
        x.productArity match
          case 1 => x.isInstanceOf[Tuple1[?]]
          case 2 => x.isInstanceOf[Tuple2[?, ?]]
          case 3 => x.isInstanceOf[Tuple3[?, ?, ?]]
          case 4 => x.isInstanceOf[Tuple4[?, ?, ?, ?]]
          case 5 => x.isInstanceOf[Tuple5[?, ?, ?, ?, ?]]
          case 6 => x.isInstanceOf[Tuple6[?, ?, ?, ?, ?, ?]]
          case 7 => x.isInstanceOf[Tuple7[?, ?, ?, ?, ?, ?, ?]]
          case 8 => x.isInstanceOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?]]
          case 9 => x.isInstanceOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 10 => x.isInstanceOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 11 => x.isInstanceOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 12 => x.isInstanceOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 13 => x.isInstanceOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 14 => x.isInstanceOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 15 => x.isInstanceOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 16 => x.isInstanceOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 17 => x.isInstanceOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 18 => x.isInstanceOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 19 => x.isInstanceOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 20 => x.isInstanceOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 21 => x.isInstanceOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 22 => x.isInstanceOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case _ => x.isInstanceOf[TupleXXL]
      case _ =>
        false

}
