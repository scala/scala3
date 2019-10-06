
import scala.quoted._
import scala.quoted.staging._
import scala.quoted.autolift.given

import liftable.Units._
import liftable.Lets._
import liftable.Loops._
import liftable.Lists._
import liftable.Exprs._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val liftedUnit: Expr[Unit] = '{}

    letVal('{1})(a => '{ $a + 1 }).show
    letLazyVal('{1})(a => '{ $a + 1 }).show
    letDef('{1})(a => '{ $a + 1 }).show

    liftedWhile('{true})('{ println(1) }).show
    liftedDoWhile('{ println(1) })('{true}).show

    val t1: Expr[Tuple1[Int]] = Tuple1(4)
    val t2: Expr[(Int, Int)] = (2, 3)
    val t3: Expr[(Int, Int, Int)] = (2, 3, 4)
    val t4: Expr[(Int, Int, Int, Int)] = (2, 3, 4, 5)
    Expr((1, 2, 3, 4))
    Expr((1, 2, 3, 4, 5))
    Expr((1, 2, 3, 4, 5, 6))
    Expr((1, 2, 3, 4, 5, 6, 7))
    Expr((1, 2, 3, 4, 5, 6, 7, 8))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24))
    Expr((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25))

    val list: List[Int] = List(1, 2, 3)
    val liftedList: Expr[List[Int]] = list

    val seq: Seq[Int] = Seq(1, 2, 3)
    val liftedSeq: Expr[Seq[Int]] = seq

    val set: Set[Int] = Set(1, 2, 3)
    val liftedSet: Expr[Set[Int]] = set

    val map: Map[Int, Char] = Map(1 -> 'a', 2 -> 'b', 3 -> 'c')
    val liftedMap: Expr[Map[Int, Char]] = map

    liftedList.foldLeft[Int](0)('{ (acc: Int, x: Int) => acc + x }).show
    liftedList.foreach('{ (x: Int) => println(x) }).show

    list.unrolledFoldLeft[Int](0)('{ (acc: Int, x: Int) => acc + x }).show
    list.unrolledForeach('{ (x: Int) => println(x) }).show

    val iarray: IArray[Int] = IArray(1, 2, 3)
    val liftedIArray: Expr[IArray[Int]] = iarray

    val iarray2: IArray[String] = IArray("a", "b", "c")
    Expr(iarray2)

    Expr(IArray(false))
    Expr(IArray(1: Byte))
    Expr(IArray(1: Short))
    Expr(IArray(1))
    Expr(IArray(1L))
    Expr(IArray(1.1f))
    Expr(IArray(1.1d))
    Expr(IArray('a'))
    Expr(IArray((1, 3)))

    val array: Array[Int] = Array(1, 2, 3)
    val liftedArray: Expr[Array[Int]] = array

    Expr(Array(false))
    Expr(Array(1: Byte))
    Expr(Array(1: Short))
    Expr(Array(1))
    Expr(Array(1L))
    Expr(Array(1.1f))
    Expr(Array(1.1d))
    Expr(Array('a'))
    Expr(Array((1, 3)))

    val some: Option[Int] = Some(2)
    val none: Option[Int] = Some(2)
    val liftedSome: Expr[Option[Int]] = some
    val liftedNone: Expr[Option[Int]] = none

    val left: Either[Int, Long] = Left(1)
    val right: Either[Int, Long] = Right(2L)
    Expr(left)
    Expr(right)

    println("quote lib ok")
  }
}


package liftable {
  import scala.quoted.Liftable
  import scala.reflect.ClassTag

  object Exprs {
    implicit class LiftExprOps[T](x: T) extends AnyVal {
      def toExpr(given Liftable[T], QuoteContext): Expr[T] =
        summon[Liftable[T]].toExpr(x)
    }
  }

  object Units {
    implicit def UnitIsLiftable: Liftable[Unit] = new Liftable[Unit] {
      def toExpr(x: Unit) = '{}
    }
  }

  object Lets {
    def letVal[T, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit t: Type[T], qctx: QuoteContext): Expr[U] =
      '{ val letVal: $t = $expr; ${ body('letVal) } }
    def letLazyVal[T, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit t: Type[T], qctx: QuoteContext): Expr[U] =
      '{ lazy val letLazyVal: $t = $expr; ${ body('letLazyVal) } }
    def letDef[T, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit t: Type[T], qctx: QuoteContext): Expr[U] =
      '{ def letDef: $t = $expr; ${ body('letDef) } }
  }

  object Loops {
    def liftedWhile(cond: Expr[Boolean])(body: Expr[Unit])(given QuoteContext): Expr[Unit] = '{ while ($cond) $body }
    def liftedDoWhile(body: Expr[Unit])(cond: Expr[Boolean])(given QuoteContext): Expr[Unit] = '{ while { $body ; $cond } do () }
  }


  object Lists {

    implicit class LiftedOps[T: Liftable](list: Expr[List[T]])(implicit t: Type[T]) {
      def foldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U], qctx: QuoteContext): Expr[U] =
        '{ ($list).foldLeft[$u]($acc)($f) }
      def foreach(f: Expr[T => Unit])(given QuoteContext): Expr[Unit] =
        '{ ($list).foreach($f) }
    }

    implicit class UnrolledOps[T: Liftable](list: List[T])(implicit t: Type[T], qctx: QuoteContext) {
      def unrolledFoldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U]): Expr[U] = list match {
        case x :: xs => xs.unrolledFoldLeft('{ ($f).apply($acc, ${x}) })(f)
        case Nil => acc
      }
       def unrolledForeach(f: Expr[T => Unit]): Expr[Unit] = list match {
         case x :: xs => '{ ($f).apply(${x}); ${ xs.unrolledForeach(f) } }
         case Nil => '{}
       }
    }

  }
}
