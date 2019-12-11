package scala.runtime

import scala.Tuple.{ Concat, Size, Head, Tail, Elem, Zip, Map }
import scala.reflect.ClassTag

object DynamicTuple {

  given iarrayOps: [T](arr: IArray[T]) {
    def ++[U >: T: ClassTag](that: IArray[U]): IArray[U] =
      (arr.asInstanceOf[Array[T]] ++ that.asInstanceOf[Array[U]]).asInstanceOf

    def contains(elem: T): Boolean =
      arr.asInstanceOf[Array[T]].contains(elem)

    def count(p: T => Boolean): Int =
      arr.asInstanceOf[Array[T]].count(p)

    def drop(n: Int): IArray[T] =
      arr.asInstanceOf[Array[T]].drop(n).asInstanceOf

    def dropRight(n: Int): IArray[T] =
      arr.asInstanceOf[Array[T]].dropRight(n).asInstanceOf

    def dropWhile(p: T => Boolean): IArray[T] =
      arr.asInstanceOf[Array[T]].dropWhile(p).asInstanceOf

    def exists(p: T => Boolean): IArray[T] =
      arr.asInstanceOf[Array[T]].exists(p).asInstanceOf

    def filter(p: T => Boolean): IArray[T] =
      arr.asInstanceOf[Array[T]].filter(p).asInstanceOf

    def filterNot(p: T => Boolean): IArray[T] =
      arr.asInstanceOf[Array[T]].filterNot(p).asInstanceOf

    def find(p: T => Boolean): Option[T] =
      arr.asInstanceOf[Array[T]].find(p).asInstanceOf

    def flatMap[U: ClassTag](f: T => IterableOnce[U]): IArray[U] =
      arr.asInstanceOf[Array[T]].flatMap(f).asInstanceOf

    def flatten[U: ClassTag](given T => Iterable[U]): IArray[U] =
      arr.asInstanceOf[Array[T]].flatten.asInstanceOf

    def fold[U >: T: ClassTag](z: U)(op: (U, U) => U): U =
      arr.asInstanceOf[Array[T]].fold(z)(op).asInstanceOf

    def foldLeft[U >: T: ClassTag](z: U)(op: (U, T) => U): U =
      arr.asInstanceOf[Array[T]].foldLeft(z)(op).asInstanceOf

    def foldRight[U >: T: ClassTag](z: U)(op: (T, U) => U): U =
      arr.asInstanceOf[Array[T]].foldRight(z)(op).asInstanceOf

    def forall(p: T => Boolean): Boolean =
      arr.asInstanceOf[Array[T]].forall(p)

    def foreach[U](f: T => U): Unit =
      arr.asInstanceOf[Array[T]].foreach(f)

    def head: T =
      arr.asInstanceOf[Array[T]].head

    def headOption: Option[T] =
      arr.asInstanceOf[Array[T]].headOption

    def indexOf(elem: T, from: Int = 0): Int =
      arr.asInstanceOf[Array[T]].indexOf(elem, from)

    def indexWhere(p: T => Boolean, from: Int = 0): Int =
      arr.asInstanceOf[Array[T]].indexWhere(p, from)

    def indices: Range =
      arr.asInstanceOf[Array[T]].indices.asInstanceOf

    def init: IArray[T] =
      arr.asInstanceOf[Array[T]].init.asInstanceOf

    def isEmpty: Boolean =
      arr.asInstanceOf[Array[T]].isEmpty

    def iterator: Iterator[T] =
      arr.asInstanceOf[Array[T]].iterator

    def last: T =
      arr.asInstanceOf[Array[T]].last

    def lastOption: Option[T] =
      arr.asInstanceOf[Array[T]].lastOption

    def lastIndexOf(elem: T, from: Int = 0): Int =
      arr.asInstanceOf[Array[T]].lastIndexOf(elem, from)

    def lastIndexWhere(p: T => Boolean, from: Int = 0): Int =
      arr.asInstanceOf[Array[T]].lastIndexWhere(p, from)

    def map[U: ClassTag](f: T => U): IArray[U] =
      arr.asInstanceOf[Array[T]].map(f).asInstanceOf

    def nonEmpty: Boolean =
      arr.asInstanceOf[Array[T]].nonEmpty

    def partition(p: T => Boolean): (IArray[T], IArray[T]) = arr.asInstanceOf[Array[T]].partition(p) match {
      case (x, y) => (x.asInstanceOf[IArray[T]], y.asInstanceOf[IArray[T]])
    }

    def reverse: IArray[T] =
      arr.asInstanceOf[Array[T]].reverse.asInstanceOf

    def scan[U >: T: ClassTag](z: U)(op: (U, U) => U): Array[U] =
      arr.asInstanceOf[Array[T]].scan(z)(op).asInstanceOf

    def scanLeft[U: ClassTag](z: U)(op: (U, T) => U): Array[U] =
      arr.asInstanceOf[Array[T]].scanLeft(z)(op).asInstanceOf

    def scanRight[U: ClassTag](z: U)(op: (T, U) => U): Array[U] =
      arr.asInstanceOf[Array[T]].scanRight(z)(op).asInstanceOf

    def size: Int =
      arr.asInstanceOf[Array[T]].size

    def slice(from: Int, until: Int): Array[T] =
      arr.asInstanceOf[Array[T]].slice(from, until).asInstanceOf

    def sortBy[U: ClassTag](f: T => U)(given math.Ordering[U]): IArray[T] =
      arr.asInstanceOf[Array[T]].sortBy(f).asInstanceOf

    def sortWith(f: (T, T) => Boolean): IArray[T] =
      arr.asInstanceOf[Array[T]].sortWith(f).asInstanceOf

    def sorted(given math.Ordering[T]): IArray[T] =
      arr.asInstanceOf[Array[T]].sorted.asInstanceOf

    def span(p: T => Boolean): (IArray[T], IArray[T]) = arr.asInstanceOf[Array[T]].span(p) match {
      case (x, y) => (x.asInstanceOf[IArray[T]], y.asInstanceOf[IArray[T]])
    }

    def splitAt(n: Int): (IArray[T], IArray[T]) = arr.asInstanceOf[Array[T]].splitAt(n) match {
      case (x, y) => (x.asInstanceOf[IArray[T]], y.asInstanceOf[IArray[T]])
    }

    def startsWith[U >: T: ClassTag](that: IArray[U], offset: Int = 0): Boolean =
      arr.asInstanceOf[Array[T]].startsWith(that.asInstanceOf[Array[U]])

    def tail: IArray[T] =
      arr.asInstanceOf[Array[T]].tail.asInstanceOf

    def take(n: Int): IArray[T] =
      arr.asInstanceOf[Array[T]].take(n).asInstanceOf

    def takeRight(n: Int): IArray[T] =
      arr.asInstanceOf[Array[T]].takeRight(n).asInstanceOf

    def takeWhile(p: T => Boolean): IArray[T] =
      arr.asInstanceOf[Array[T]].takeWhile(p).asInstanceOf

    def unzip[U: ClassTag, V: ClassTag](given T => (U, V)): (IArray[U], IArray[V]) =
      arr.asInstanceOf[Array[T]].unzip(asPair, ct1, ct2) match {
        case (x, y) => (x.asInstanceOf[IArray[U]], y.asInstanceOf[IArray[V]])
      }

    def zip[U: ClassTag](that: IterableOnce[U]): IArray[(T, U)] =
      arr.asInstanceOf[Array[T]].zip(that).asInstanceOf
  }

  inline val MaxSpecialized = 22
  inline private val XXL = MaxSpecialized + 1

  def to$Array(xs: Tuple, n: Int) = {
    val arr = new Array[Object](n)
    var i = 0
    var it = xs.asInstanceOf[Product].productIterator
    while (i < n) {
      arr(i) = it.next().asInstanceOf[Object]
      i += 1
    }
    arr
  }

  def cons$Array[H](x: H, elems: Array[Object]): Array[Object] = {
    val elems1 = new Array[Object](elems.length + 1)
    elems1(0) = x.asInstanceOf[Object]
    System.arraycopy(elems, 0, elems1, 1, elems.length)
    elems1
  }

  def dynamicFromArray[T <: Tuple](xs: Array[Object]): T = xs.length match {
    case 0  => ().asInstanceOf[T]
    case 1  => Tuple1(xs(0)).asInstanceOf[T]
    case 2  => Tuple2(xs(0), xs(1)).asInstanceOf[T]
    case 3  => Tuple3(xs(0), xs(1), xs(2)).asInstanceOf[T]
    case 4  => Tuple4(xs(0), xs(1), xs(2), xs(3)).asInstanceOf[T]
    case 5  => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4)).asInstanceOf[T]
    case 6  => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5)).asInstanceOf[T]
    case 7  => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6)).asInstanceOf[T]
    case 8  => Tuple8(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7)).asInstanceOf[T]
    case 9  => Tuple9(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8)).asInstanceOf[T]
    case 10 => Tuple10(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9)).asInstanceOf[T]
    case 11 => Tuple11(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10)).asInstanceOf[T]
    case 12 => Tuple12(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11)).asInstanceOf[T]
    case 13 => Tuple13(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12)).asInstanceOf[T]
    case 14 => Tuple14(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13)).asInstanceOf[T]
    case 15 => Tuple15(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14)).asInstanceOf[T]
    case 16 => Tuple16(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15)).asInstanceOf[T]
    case 17 => Tuple17(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16)).asInstanceOf[T]
    case 18 => Tuple18(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17)).asInstanceOf[T]
    case 19 => Tuple19(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18)).asInstanceOf[T]
    case 20 => Tuple20(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19)).asInstanceOf[T]
    case 21 => Tuple21(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20)).asInstanceOf[T]
    case 22 => Tuple22(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20), xs(21)).asInstanceOf[T]
    case _ => TupleXXL.fromIArray(xs.clone().asInstanceOf[IArray[Object]]).asInstanceOf[T]
  }

  def dynamicFromIArray[T <: Tuple](xs: IArray[Object]): T =
    if (xs.length <= 22) dynamicFromArray(xs.asInstanceOf[Array[Object]])
    else TupleXXL.fromIArray(xs).asInstanceOf[T]

  def dynamicFromProduct[T <: Tuple](xs: Product): T = (xs.productArity match {
    case 1 =>
      xs match {
        case xs: Tuple1[_] => xs
        case xs => Tuple1(xs.productElement(0))
      }
    case 2 =>
      xs match {
        case xs: Tuple2[_, _] => xs
        case xs => Tuple2(xs.productElement(0), xs.productElement(1))
      }
    case 3 =>
      xs match {
        case xs: Tuple3[_, _, _] => xs
        case xs => Tuple3(xs.productElement(0), xs.productElement(1), xs.productElement(2))
      }
    case 4 =>
      xs match {
        case xs: Tuple4[_, _, _, _] => xs
        case xs => Tuple4(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3))
      }
    case 5 =>
      xs match {
        case xs: Tuple5[_, _, _, _, _] => xs
        case xs => Tuple5(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4))
      }
    case 6 =>
      xs match {
        case xs: Tuple6[_, _, _, _, _, _] => xs
        case xs => Tuple6(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5))
      }
    case 7 =>
      xs match {
        case xs: Tuple7[_, _, _, _, _, _, _] => xs
        case xs => Tuple7(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6))
      }
    case 8 =>
      xs match {
        case xs: Tuple8[_, _, _, _, _, _, _, _] => xs
        case xs => Tuple8(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7))
      }
    case 9 =>
      xs match {
        case xs: Tuple9[_, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple9(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8))
      }
    case 10 =>
      xs match {
        case xs: Tuple10[_, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple10(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9))
      }
    case 11 =>
      xs match {
        case xs: Tuple11[_, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple11(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10))
      }
    case 12 =>
      xs match {
        case xs: Tuple12[_, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple12(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11))
      }
    case 13 =>
      xs match {
        case xs: Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple13(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12))
      }
    case 14 =>
      xs match {
        case xs: Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple14(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13))
      }
    case 15 =>
      xs match {
        case xs: Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple15(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14))
      }
    case 16 =>
      xs match {
        case xs: Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple16(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15))
      }
    case 17 =>
      xs match {
        case xs: Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple17(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16))
      }
    case 18 =>
      xs match {
        case xs: Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple18(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17))
      }
    case 19 =>
      xs match {
        case xs: Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple19(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18))
      }
    case 20 =>
      xs match {
        case xs: Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple20(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19))
      }
    case 21 =>
      xs match {
        case xs: Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple21(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20))
      }
    case 22 =>
      xs match {
        case xs: Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple22(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20), xs.productElement(21))
      }
    case _ =>
      xs match {
        case xs: TupleXXL => xs
        case xs => TupleXXL.fromIArray(xs.productIterator.map(_.asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use Iterator.toIArray
      }
  }).asInstanceOf[T]


  def dynamicToArray(self: Tuple): Array[Object] = (self: Any) match {
    case self: Unit => Array.emptyObjectArray
    case self: TupleXXL => self.toArray
    case self: Product => productToArray(self)
  }

  def dynamicToIArray(self: Tuple): IArray[Object] = (self: Any) match {
    case self: Unit => Array.emptyObjectArray.asInstanceOf[IArray[Object]] // TODO use IArray.emptyObjectIArray
    case self: TupleXXL => self.elems
    case self: Product => productToArray(self).asInstanceOf[IArray[Object]]
  }

  def productToArray(self: Product): Array[Object] = {
    val arr = new Array[Object](self.productArity)
    for (i <- 0 until arr.length) arr(i) = self.productElement(i).asInstanceOf[Object]
    arr
  }

  def dynamicCons[H, This <: Tuple](x: H, self: Tuple): H *: This = {
    type Result = H *: This
    (self: Any) match {
      case () =>
        Tuple1(x).asInstanceOf[Result]
      case self: Tuple1[_] =>
        Tuple2(x, self._1).asInstanceOf[Result]
      case self: Tuple2[_, _] =>
        Tuple3(x, self._1, self._2).asInstanceOf[Result]
      case self: Tuple3[_, _, _] =>
        Tuple4(x, self._1, self._2, self._3).asInstanceOf[Result]
      case self: Tuple4[_, _, _, _] =>
        Tuple5(x, self._1, self._2, self._3, self._4).asInstanceOf[Result]
      case _ =>
        dynamicFromArray[Result](cons$Array(x, dynamicToArray(self)))
    }
  }

  def dynamicConcat[This <: Tuple, That <: Tuple](self: This, that: That): Concat[This, That] = {
    type Result = Concat[This, That]
    (self: Any) match {
      case self: Unit => return that.asInstanceOf[Result]
      case _ =>
    }
    (that: Any) match {
      case that: Unit => return self.asInstanceOf[Result]
      case _ =>
    }
    dynamicFromArray[Result](dynamicToArray(self) ++ dynamicToArray(that))
  }

  def dynamicSize[This <: Tuple](self: This): Size[This] = (self: Any) match {
    case self: Unit => 0.asInstanceOf[Size[This]]
    case self: Product => self.productArity.asInstanceOf[Size[This]]
  }

  def dynamicTail[This <: NonEmptyTuple] (self: This): Tail[This] = {
    type Result = Tail[This]
    val res = (self: Any) match {
      case self: Tuple1[_] => ()
      case self: Tuple2[_, _] => Tuple1(self._2)
      case self: Tuple3[_, _, _] => Tuple2(self._2, self._3)
      case self: Tuple4[_, _, _, _] => Tuple3(self._2, self._3, self._4)
      case _ => dynamicFromArray[Result](dynamicToArray(self).tail)
    }
    res.asInstanceOf[Result]
  }

  def dynamicApply[This <: NonEmptyTuple, N <: Int] (self: This, n: Int): Elem[This, N] = {
    type Result = Elem[This, N]
    val res = (self: Any) match {
      case self: Unit => throw new IndexOutOfBoundsException(n.toString)
      case self: Product => self.productElement(n)
    }
    res.asInstanceOf[Result]
  }

  def dynamicZip[This <: Tuple, T2 <: Tuple](t1: This, t2: T2): Zip[This, T2] = {
    if (t1.size == 0 || t2.size == 0) ().asInstanceOf[Zip[This, T2]]
    else Tuple.fromArray(
      t1.asInstanceOf[Product].productIterator.zip(
      t2.asInstanceOf[Product].productIterator).toArray // TODO use toIArray of Object to avoid double/triple array copy
    ).asInstanceOf[Zip[This, T2]]
  }

  def dynamicMap[This <: Tuple, F[_]](self: This, f: [t] => t => F[t]): Map[This, F] = (self: Any) match {
    case self: Unit => ().asInstanceOf[Map[This, F]]
    case _ =>
      Tuple.fromArray(self.asInstanceOf[Product].productIterator.map(f(_)).toArray) // TODO use toIArray of Object to avoid double/triple array copy
        .asInstanceOf[Map[This, F]]
  }

  def consIterator(head: Any, tail: Tuple): Iterator[Any] =
    Iterator.single(head) ++ tail.asInstanceOf[Product].productIterator

  def concatIterator(tup1: Tuple, tup2: Tuple): Iterator[Any] =
    tup1.asInstanceOf[Product].productIterator ++ tup2.asInstanceOf[Product].productIterator
}
