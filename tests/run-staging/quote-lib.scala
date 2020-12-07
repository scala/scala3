
import scala.quoted._
import scala.quoted.staging._

import liftable.Units._
import liftable.Lets._
import liftable.Loops._
import liftable.Lists._
import liftable.Exprs._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    val liftedUnit: Expr[Unit] = '{}

    letVal('{1})(a => '{ $a + 1 }).show
    letLazyVal('{1})(a => '{ $a + 1 }).show
    letDef('{1})(a => '{ $a + 1 }).show

    liftedWhile('{true})('{ println(1) }).show
    liftedDoWhile('{ println(1) })('{true}).show

    val t1: Expr[Tuple1[Int]] = Value(Tuple1(4))
    val t2: Expr[(Int, Int)] = Value((2, 3))
    val t3: Expr[(Int, Int, Int)] = Value((2, 3, 4))
    val t4: Expr[(Int, Int, Int, Int)] = Value((2, 3, 4, 5))
    Value((1, 2, 3, 4))
    Value((1, 2, 3, 4, 5))
    Value((1, 2, 3, 4, 5, 6))
    Value((1, 2, 3, 4, 5, 6, 7))
    Value((1, 2, 3, 4, 5, 6, 7, 8))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24))
    Value((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25))

    val list: List[Int] = List(1, 2, 3)
    val liftedList: Expr[List[Int]] = Value(list)

    val seq: Seq[Int] = Seq(1, 2, 3)
    val liftedSeq: Expr[Seq[Int]] = Value(seq)

    val set: Set[Int] = Set(1, 2, 3)
    val liftedSet: Expr[Set[Int]] = Value(set)

    val map: Map[Int, Char] = Map(1 -> 'a', 2 -> 'b', 3 -> 'c')
    val liftedMap: Expr[Map[Int, Char]] = Value(map)

    liftedList.foldLeft[Int]('{0})('{ (acc: Int, x: Int) => acc + x }).show
    liftedList.foreach('{ (x: Int) => println(x) }).show

    list.unrolledFoldLeft[Int]('{0})('{ (acc: Int, x: Int) => acc + x }).show
    list.unrolledForeach('{ (x: Int) => println(x) }).show

    val iarray: IArray[Int] = IArray(1, 2, 3)
    val liftedIArray: Expr[IArray[Int]] = Value(iarray)

    val iarray2: IArray[String] = IArray("a", "b", "c")
    Value(iarray2)

    Value(IArray(false))
    Value(IArray(1: Byte))
    Value(IArray(1: Short))
    Value(IArray(1))
    Value(IArray(1L))
    Value(IArray(1.1f))
    Value(IArray(1.1d))
    Value(IArray('a'))
    Value(IArray((1, 3)))

    val array: Array[Int] = Array(1, 2, 3)
    val liftedArray: Expr[Array[Int]] = Value(array)

    Value(Array(false))
    Value(Array(1: Byte))
    Value(Array(1: Short))
    Value(Array(1))
    Value(Array(1L))
    Value(Array(1.1f))
    Value(Array(1.1d))
    Value(Array('a'))
    Value(Array((1, 3)))

    val some: Option[Int] = Some(2)
    val none: Option[Int] = Some(2)
    val liftedSome: Expr[Option[Int]] = Value(some)
    val liftedNone: Expr[Option[Int]] = Value(none)

    val left: Either[Int, Long] = Left(1)
    val right: Either[Int, Long] = Right(2L)
    Value(left)
    Value(right)

    println("quote lib ok")
  }
}


package liftable {
  import scala.quoted.ToExpr
  import scala.reflect.ClassTag

  object Exprs {
    implicit class LiftExprOps[T](x: T) extends AnyVal {
      def apply(using ToExpr[T], Quotes): Expr[T] =
        summon[ToExpr[T]].apply(x)
    }
  }

  object Units {
    implicit def UnitIsToExpr: ToExpr[Unit] = new ToExpr[Unit] {
      def apply(x: Unit)(using Quotes) = '{}
    }
  }

  object Lets {
    def letVal[T: Type, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit qctx: Quotes): Expr[U] =
      '{ val letVal: T = $expr; ${ body('letVal) } }
    def letLazyVal[T: Type, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit qctx: Quotes): Expr[U] =
      '{ lazy val letLazyVal: T = $expr; ${ body('letLazyVal) } }
    def letDef[T: Type, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit qctx: Quotes): Expr[U] =
      '{ def letDef: T = $expr; ${ body('letDef) } }
  }

  object Loops {
    def liftedWhile(cond: Expr[Boolean])(body: Expr[Unit])(using Quotes): Expr[Unit] = '{ while ($cond) $body }
    def liftedDoWhile(body: Expr[Unit])(cond: Expr[Boolean])(using Quotes): Expr[Unit] = '{ while { $body ; $cond } do () }
  }


  object Lists {

    implicit class LiftedOps[T: ToExpr](list: Expr[List[T]])(implicit t: Type[T]) {
      def foldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U], qctx: Quotes): Expr[U] =
        '{ ($list).foldLeft[U]($acc)($f) }
      def foreach(f: Expr[T => Unit])(using Quotes): Expr[Unit] =
        '{ ($list).foreach($f) }
    }

    implicit class UnrolledOps[T: ToExpr](list: List[T])(implicit t: Type[T], qctx: Quotes) {
      def unrolledFoldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U]): Expr[U] = list match {
        case x :: xs => xs.unrolledFoldLeft('{ ($f).apply($acc, ${Value(x)}) })(f)
        case Nil => acc
      }
       def unrolledForeach(f: Expr[T => Unit]): Expr[Unit] = list match {
         case x :: xs => '{ ($f).apply(${Value(x)}); ${ xs.unrolledForeach(f) } }
         case Nil => '{}
       }
    }

  }
}
