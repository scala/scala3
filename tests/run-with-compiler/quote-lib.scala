
import scala.quoted._
import scala.quoted.autolift._

import liftable.Units._
import liftable.Lets._
import liftable.Loops._
import liftable.Tuples._
import liftable.Lists._
import liftable.Exprs._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
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

    val list: List[Int] = List(1, 2, 3)
    val liftedList: Expr[List[Int]] = list

    liftedList.foldLeft[Int](0)('{ (acc: Int, x: Int) => acc + x }).show
    liftedList.foreach('{ (x: Int) => println(x) }).show

    list.unrolledFoldLeft[Int](0)('{ (acc: Int, x: Int) => acc + x }).show
    list.unrolledForeach('{ (x: Int) => println(x) }).show

    println("quote lib ok")
  }
}


package liftable {
  import scala.quoted.Liftable
  import scala.reflect.ClassTag

  object Exprs {
    implicit class LiftExprOps[T](x: T) extends AnyVal {
      def toExpr(implicit liftable: Liftable[T]): Expr[T] =
        liftable.toExpr(x)
    }
  }

  object Units {
    implicit def UnitIsLiftable: Liftable[Unit] = new Liftable[Unit] {
      def toExpr(x: Unit): Expr[Unit] = '{}
    }
  }

  object Lets {
    def letVal[T, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit t: Type[T]): Expr[U] =
      '{ val letVal: $t = $expr; ${ body('letVal) } }
    def letLazyVal[T, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit t: Type[T]): Expr[U] =
      '{ lazy val letLazyVal: $t = $expr; ${ body('letLazyVal) } }
    def letDef[T, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(implicit t: Type[T]): Expr[U] =
      '{ def letDef: $t = $expr; ${ body('letDef) } }
  }

  object Loops {
    def liftedWhile(cond: Expr[Boolean])(body: Expr[Unit]): Expr[Unit] = '{ while ($cond) $body }
    def liftedDoWhile(body: Expr[Unit])(cond: Expr[Boolean]): Expr[Unit] = '{ do $body while ($cond) }
  }

  object Tuples {

    implicit def Tuple1IsLiftable[T1: Liftable](implicit t1: Type[T1]): Liftable[Tuple1[T1]] = new Liftable[Tuple1[T1]] {
      def toExpr(x: Tuple1[T1]): Expr[Tuple1[T1]] =
        '{ Tuple1[$t1](${ x._1}) }
    }

    implicit def Tuple2IsLiftable[T1: Liftable, T2: Liftable](implicit t1: Type[T1], t2: Type[T2]): Liftable[(T1, T2)] = new Liftable[(T1, T2)] {
      def toExpr(x: (T1, T2)): Expr[(T1, T2)] =
        '{ Tuple2[$t1, $t2](${x._1}, ${x._2}) }

    }

    implicit def Tuple3IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3]): Liftable[(T1, T2, T3)] = new Liftable[(T1, T2, T3)] {
      def toExpr(x: (T1, T2, T3)): Expr[(T1, T2, T3)] =
        '{ Tuple3[$t1, $t2, $t3](${x._1}, ${x._2}, ${x._3}) }

    }

    implicit def Tuple4IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4]): Liftable[(T1, T2, T3, T4)] = new Liftable[(T1, T2, T3, T4)] {
      def toExpr(x: (T1, T2, T3, T4)): Expr[(T1, T2, T3, T4)] =
        '{ Tuple4[$t1, $t2, $t3, $t4](${x._1}, ${x._2}, ${x._3}, ${x._4}) }
    }

    // TODO more tuples

  }


  object Lists {
    implicit def ListIsLiftable[T: Liftable](implicit t: Type[T]): Liftable[List[T]] = new Liftable[List[T]] {
      def toExpr(x: List[T]): Expr[List[T]] = x match {
        case x :: xs  => '{ (${xs}).::[$t](${x}) }
        case Nil => '{ Nil: List[$t] }
      }
    }

    implicit class LiftedOps[T: Liftable](list: Expr[List[T]])(implicit t: Type[T]) {
      def foldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U]): Expr[U] =
        '{ ($list).foldLeft[$u]($acc)($f) }
      def foreach(f: Expr[T => Unit]): Expr[Unit] =
        '{ ($list).foreach($f) }
    }

    implicit class UnrolledOps[T: Liftable](list: List[T])(implicit t: Type[T]) {
      def unrolledFoldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U]): Expr[U] = list match {
        case x :: xs => xs.unrolledFoldLeft('{ ($f).apply($acc, ${x}) })(f)
        case Nil => acc
      }
       def unrolledForeach(f: Expr[T => Unit]): Expr[Unit] = list match {
         case x :: xs => '{ ($f).apply(${x}); ${ xs.unrolledForeach(f) } }
         case Nil => '{}
       }
    }

    object Arrays {
      implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T], ct: Expr[ClassTag[T]]): Liftable[Array[T]] = new Liftable[Array[T]] {
        def toExpr(arr: Array[T]): Expr[Array[T]] = '{ new Array[$t](${arr.length})($ct) }
      }
    }

  }
}
