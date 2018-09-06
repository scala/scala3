package scala
import annotation.showAsInfix
import typelevel._

sealed trait Tuple extends Any {
  import Tuple._

  rewrite def toArray: Array[Object] = ~Macros.toArrayImpl('(this), constValue[BoundedSize[this.type]])

  rewrite def *: [H] (x: H): H *: this.type = ~Macros.consImpl[H, H *: this.type]('(x), '(this), constValue[BoundedSize[this.type]])

  rewrite def ++(that: Tuple): Concat[this.type, that.type] = ~Macros.plusPlusImpl[Concat[this.type, that.type]]('(this), '(that), constValue[BoundedSize[this.type]], constValue[BoundedSize[that.type]])

}

object Tuple {
  transparent val $MaxSpecialized = 22
  transparent private val XXL = $MaxSpecialized + 1

  type Head[X <: NonEmptyTuple] = X match {
    case x *: _ => x
  }

  type Tail[X <: NonEmptyTuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  type Concat[X <: Tuple, Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[X <: Tuple, N] = (X, N) match {
    case (x *: xs, 0) => x
    case (x *: xs, S[n1]) => Elem[xs, n1]
  }

  type Size[X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Size[xs]]
  }

  private type XXL = S[$MaxSpecialized.type]

  private type BoundedS[N <: Int] = N match {
    case XXL => XXL
    case _ => S[N]
  }

  private[scala] type BoundedSize[X] <: Int = X match {
    case Unit => 0
    case x *: xs => BoundedSize[xs] match {
      case XXL => XXL
      case _ => S[BoundedSize[xs]]
    }
  }

  val $emptyArray = Array[Object]()

  def $toArray(xs: Tuple, n: Int) = {
    val arr = new Array[Object](n)
    var i = 0
    var it = xs.asInstanceOf[Product].productIterator
    while (i < n) {
      arr(i) = it.next().asInstanceOf[Object]
      i += 1
    }
    arr
  }

  def $consArray[H](x: H, elems: Array[Object]): Array[Object] = {
    val elems1 = new Array[Object](elems.length + 1)
    elems1(0) = x.asInstanceOf[Object]
    Array.copy(elems, 0, elems1, 1, elems.length)
    elems1
  }

  rewrite def fromArray[T <: Tuple](xs: Array[Object]): T = ~Macros.fromArrayImpl[T]('(xs), constValue[BoundedSize[T]])

}

abstract sealed class NonEmptyTuple extends Tuple {
  import Tuple._

  // TODO
//  rewrite def head: Head[this.type] = {
//    type Result = Head[this.type]
//    val resVal = rewrite constValueOpt[BoundedSize[this.type]] match {
//      case Some(1) =>
//        val t = asInstanceOf[Tuple1[_]]
//        t._1
//      case Some(2) =>
//        val t = asInstanceOf[Tuple2[_, _]]
//        t._1
//      case Some(3) =>
//        val t = asInstanceOf[Tuple3[_, _, _]]
//        t._1
//      case Some(4) =>
//        val t = asInstanceOf[Tuple4[_, _, _, _]]
//        t._1
//      case Some(n) if n > 4 && n <= $MaxSpecialized =>
//        asInstanceOf[Product].productElement(0)
//      case Some(n) if n > $MaxSpecialized =>
//        val t = asInstanceOf[TupleXXL]
//        t.elems(0)
//      case None =>
//        error(".head cannot be applied to tuple of unknown size")
//    }
//    resVal.asInstanceOf[Result]
//  }
//
//  rewrite def tail: Tail[this.type] = {
//    type Result = Tail[this.type]
//    rewrite constValueOpt[BoundedSize[this.type]]  match {
//      case Some(1) =>
//        ().asInstanceOf[Result]
//      case Some(2) =>
//        val t = asInstanceOf[Tuple2[_, _]]
//        Tuple1(t._2).asInstanceOf[Result]
//      case Some(3) =>
//        val t = asInstanceOf[Tuple3[_, _, _]]
//        Tuple2(t._2, t._3).asInstanceOf[Result]
//      case Some(4) =>
//        val t = asInstanceOf[Tuple4[_, _, _, _]]
//        Tuple3(t._2, t._3, t._4).asInstanceOf[Result]
//      case Some(5) =>
//        val t = asInstanceOf[Tuple5[_, _, _, _, _]]
//        Tuple4(t._2, t._3, t._4, t._5).asInstanceOf[Result]
//      case Some(n) if n > 5 =>
//        fromArray[Result](toArray.tail)
//      case None =>
//        error(".tail cannot be applied to tuple of unknown size")
//    }
//  }

  rewrite def apply(transparent n: Int): Elem[this.type, n.type] = ~Macros.appyImpl[Elem[this.type, n.type]]('(this), constValue[BoundedSize[this.type]], n)

}

object Macros {
  import scala.quoted._

  def error(msg: String): Nothing = throw new QuoteError(msg)

  def toArrayImpl[T <: Tuple : Type](tuple: Expr[Tuple], size: Int): Expr[Array[Object]] = {
    size match {
      case 0 => '(Array.empty[Object])
      case 1 =>
        '{
          val t = (~tuple).asInstanceOf[Tuple1[Object]]
          Array(t._1)
        }
      case 2 =>
        '{
          val t = (~tuple).asInstanceOf[Tuple2[Object, Object]]
          Array(t._1, t._2)
        }
      case 3 =>
        '{
          val t = (~tuple).asInstanceOf[Tuple3[Object, Object, Object]]
          Array(t._1, t._2, t._3)
        }
      case 4 =>
        '{
          val t = (~tuple).asInstanceOf[Tuple4[Object, Object, Object, Object]]
          Array(t._1, t._2, t._3, t._4)
        }
      case n =>
        if (n < Tuple.$MaxSpecialized) '(Tuple.$toArray(~tuple, ~n.toExpr))
        else '((~tuple).asInstanceOf[TupleXXL].elems)
    }
    // TODO
//        error(".toArray cannot be applied to tuple of unknown size")
  }

  def consImpl[H : Type, Result <: Tuple : Type](head: Expr[H], tail: Expr[Tuple], size: Int): Expr[Result] = {
    size match {
      case 0 =>
        '(Tuple1(~head).asInstanceOf[Result])
      case 1 =>
        '(Tuple2(~head, (~tail).asInstanceOf[Tuple1[_]]._1).asInstanceOf[Result])
      case 2 =>
        '{
          val t = (~tail).asInstanceOf[Tuple2[_, _]]
          Tuple3(~head, t._1, t._2).asInstanceOf[Result]
        }
      case 3 =>
        '{
          val t = (~tail).asInstanceOf[Tuple3[_, _, _]]
          Tuple4(~head, t._1, t._2, t._3).asInstanceOf[Result]
        }
      case 4 =>
        '{
          val t = (~tail).asInstanceOf[Tuple4[_, _, _, _]]
          Tuple5(~head, t._1, t._2, t._3, t._4).asInstanceOf[Result]
        }
      case n =>
        fromArrayImpl('(Tuple.$consArray(~head, ~toArrayImpl(tail, n))), n + 1)
    // TODO
//        error("*: cannot be applied to tuple of unknown size")
    }
  }

  def plusPlusImpl[Result <: Tuple : Type](t1: Expr[Tuple], t2: Expr[Tuple], s1: Int, s2: Int): Expr[Result] = {
    s1 match {
      case 0 =>
        '((~t2).asInstanceOf[Result])
      case 1 =>
        if (s2 == 0) '((~t1).asInstanceOf[Result])
        else consImpl(t1, t2, 1 + s2)
      case 2 =>
        s2 match {
          case 0 => '((~t1).asInstanceOf[Result])
          case 1 =>
            '{
              val t = (~t1).asInstanceOf[Tuple2[_, _]]
              val u = (~t2).asInstanceOf[Tuple1[_]]
              Tuple3(t._1, t._2, u._1).asInstanceOf[Result]
            }
          case 2 =>
            '{
              val t = (~t1).asInstanceOf[Tuple2[_, _]]
              val u = (~t2).asInstanceOf[Tuple2[_, _]]
              Tuple4 (t._1, t._2, u._1, u._2).asInstanceOf[Result]
            }
          case _ =>
            genericConcatImpl(t1, t2, s1, s2)
        }
      case 3 =>
        s2 match {
          case 0 => '((~t1).asInstanceOf[Result])
          case 1 =>
            '{
              val t = (~t1).asInstanceOf[Tuple3[_, _, _]]
              val u = (~t2).asInstanceOf[Tuple1[_]]
              Tuple4(t._1, t._2, t._3, u._1).asInstanceOf[Result]
            }
          case _ =>
            genericConcatImpl(t1, t2, s1, s2)
        }
      case _ =>
        if (s2 == 0) '((~t1).asInstanceOf[Result])
        else genericConcatImpl(t1, t2, s1, s2)
// TODO
//        error("++ cannot be applied to tuple of unknown size")
    }
  }

  def appyImpl[Result: Type](tup: Expr[Tuple], size: Int, n: Int): Expr[Result] = size match {
    case 1 =>
      n match {
        case 0 => '((~tup).asInstanceOf[Tuple1[Result]]._1)
        case _ => error("index out of bounds")
      }
    case 2 =>
      val t = asInstanceOf[Tuple2[_, _]]
      n match {
        case 0 => '((~tup).asInstanceOf[Tuple2[Result, _]]._1)
        case 1 => '((~tup).asInstanceOf[Tuple2[_, Result]]._2)
        case _ => error("index out of bounds")
      }
    case 3 =>
      val t = asInstanceOf[Tuple3[_, _, _]]
      n match {
        case 0 => '((~tup).asInstanceOf[Tuple3[Result, _, _]]._1)
        case 1 => '((~tup).asInstanceOf[Tuple3[_, Result, _]]._2)
        case 2 => '((~tup).asInstanceOf[Tuple3[_, _, Result]]._3)
        case _ => error("index out of bounds")
      }
    case 4 =>
      val t = asInstanceOf[Tuple4[_, _, _, _]]
      n match {
        case 0 => '((~tup).asInstanceOf[Tuple4[Result, _, _, _]]._1)
        case 1 => '((~tup).asInstanceOf[Tuple4[_, Result, _, _]]._2)
        case 2 => '((~tup).asInstanceOf[Tuple4[_, _, Result, _]]._3)
        case 3 => '((~tup).asInstanceOf[Tuple4[_, _, _, Result]]._4)
        case _ => error("index out of bounds")
      }
    case s =>
      if (s > 4 && s <= Tuple.$MaxSpecialized && n >= 0 && n < s)
        '((~tup).asInstanceOf[Product].productElement(~n.toExpr).asInstanceOf[Result])
      else if (s > Tuple.$MaxSpecialized && n >= 0 && n < s)
        '((~tup).asInstanceOf[TupleXXL].elems(~n.toExpr).asInstanceOf[Result])
      else
        error("index out of bounds")
  // TODO
//        error("selection (...) cannot be applied to tuple of unknown size")
  }

  def genericConcatImpl[Result <: Tuple](t1: Expr[Tuple], t2: Expr[Tuple], s1: Int, s2: Int): Expr[Result] =
    fromArrayImpl('((~toArrayImpl(t1, s1)) ++ (~toArrayImpl(t2, s2))), s1 + s2)

  def fromArrayImpl[T <: Tuple : Type](xs: Expr[Array[Object]], size: Int): Expr[T] = size match {
    case 0  => '(().asInstanceOf[T])
    case 1  => '(Tuple1((~xs)(0)).asInstanceOf[T])
    case 2  => '(Tuple2((~xs)(0), (~xs)(1)).asInstanceOf[T])
    case 3  => '(Tuple3((~xs)(0), (~xs)(1), (~xs)(2)).asInstanceOf[T])
    case 4  => '(Tuple4((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3)).asInstanceOf[T])
    case 5  => '(Tuple5((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4)).asInstanceOf[T])
    case 6  => '(Tuple6((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5)).asInstanceOf[T])
    case 7  => '(Tuple7((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6)).asInstanceOf[T])
    case 8  => '(Tuple8((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7)).asInstanceOf[T])
    case 9  => '(Tuple9((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8)).asInstanceOf[T])
    case 10 => '(Tuple10((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9)).asInstanceOf[T])
    case 11 => '(Tuple11((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10)).asInstanceOf[T])
    case 12 => '(Tuple12((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11)).asInstanceOf[T])
    case 13 => '(Tuple13((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12)).asInstanceOf[T])
    case 14 => '(Tuple14((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13)).asInstanceOf[T])
    case 15 => '(Tuple15((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14)).asInstanceOf[T])
    case 16 => '(Tuple16((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15)).asInstanceOf[T])
    case 17 => '(Tuple17((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15), (~xs)(16)).asInstanceOf[T])
    case 18 => '(Tuple18((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15), (~xs)(16), (~xs)(17)).asInstanceOf[T])
    case 19 => '(Tuple19((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15), (~xs)(16), (~xs)(17), (~xs)(18)).asInstanceOf[T])
    case 20 => '(Tuple20((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15), (~xs)(16), (~xs)(17), (~xs)(18), (~xs)(19)).asInstanceOf[T])
    case 21 => '(Tuple21((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15), (~xs)(16), (~xs)(17), (~xs)(18), (~xs)(19), (~xs)(20)).asInstanceOf[T])
    case 22 => '(Tuple22((~xs)(0), (~xs)(1), (~xs)(2), (~xs)(3), (~xs)(4), (~xs)(5), (~xs)(6), (~xs)(7), (~xs)(8), (~xs)(9), (~xs)(10), (~xs)(11), (~xs)(12), (~xs)(13), (~xs)(14), (~xs)(15), (~xs)(16), (~xs)(17), (~xs)(18), (~xs)(19), (~xs)(20), (~xs)(21)).asInstanceOf[T])
    case _  => '(TupleXXL((~xs)).asInstanceOf[T])
  }
}

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  // TODO
//  rewrite def unapply[H, T <: Tuple](x: H *: T) = (x.head, x.tail)
}
