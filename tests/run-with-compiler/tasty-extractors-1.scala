
import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.util.TastyPrinter

object Test {
  def main(args: Array[String]): Unit = {
    for (q <- quotes) {
      val (tasty, ctx) = q.toTasty
      println(TastyPrinter.stringOf(tasty)(ctx))
      println(TastyPrinter.stringOf(tasty.tpe)(ctx))
      println()
    }
  }

  def quotes: List[Expr[_]] = List(
    '(true),
    '(1),
    '(2L),
    '(2.1f),
    '(2.2d),
    '("abc"),
    '(println("abc")),
    '(8: Int),
    '(8: Byte),
    '(8: Short),
    '( 'a' ),
    '{ 1; 2; 3 },
    '(if (true: Boolean) 1 else 2),
    '("a" match { case "a" => () }),
    '("b" match { case n => () }),
    '("c" match { case n: String => () }),
    '("e" match { case _ => () }),
    '("f" match { case _: String => () }),
    '("g" match { case _: String | _: Int => () }),
    '("h" match { case _ if false => () }),
    '{ val a = "o"; "i" match { case a => () } },
    '(Option(4) match { case Some(a) => a; case None => 1 }),
    '(Nil match { case List(a, b, c) => }),
    '(try 1 catch { case _ => }),
    '(try 2 finally ()),
    '(try 3 catch { case _ => } finally ()),
    '("a" == "b"),
    '(new Object),
    '(Int.box(x = 9)),
    '(Ordering.apply[Int]),
    '{ val a: Int = 3 },
    '{ lazy val b: Int = 3 },
    '{ def f1: Int = 3 },
    '{ def f2: Int = return 4 },
    '{ def f3(i: Int): Int = i },
    '{ def f4(i: Int)(j: Int): Int = i + j },
    '{ def f5(i: Int = 9): Int = i },
    '{ def f6[T](x: T): T = x },
    '{ def f7[T](x: T): x.type = x },
    '{ def f8(i: Int*): Int = 9; f8(1, 2, 3) },
    '{ def f9(i: => Int): Int = i },
  )
}
