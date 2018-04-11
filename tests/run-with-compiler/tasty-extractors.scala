
import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Term._

object Test {
  def main(args: Array[String]): Unit = {
    for (q <- quotes) {
      val tasty = toTasty(q)
      println(tasty)
      println(tasty.tpe)
      println()
    }
  }

  def quotes: List[Expr[_]] = List(
    '(true),
    '(1),
    '(2L),
    '(2d),
    '("abc"),
    '(println("abc")),
    '(8: Int),
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
      // TODO add unapply case
    '(try 1 catch { case _ => }),
    '(try 2 finally ()),
    '(try 3 catch { case _ => } finally ()),
    '("a" == "b"),
    '(new Object),
    '(Int.box(x = 9)),
    '(Ordering.apply[Int]),
    '{ val a: Int = 3 },
    '{ lazy val b: Int = 3 },
    '{ def c: Int = 3 },
    '{ def d: Int = return 4 },
    '{ var x = 1; x = 2 },
    '((x: Int) => x),
    '(???),
    '(1: 1),
    '(1: Int),
    '(Nil: List[Int]),
    '(1: Int & Int),
    '(1: Int | String),
    '{ import scala.collection.mutable; 1 },
    '{ import scala.collection.{mutable, immutable}; 2 },
    '{ import scala.collection.{mutable => mut}; 3 },
    '{ import scala.collection.{mutable => _}; 4 },
    '{ class Foo },
    '{ object Foo },
    '{ type Foo },
    '{ type Foo = Int },
    '{ type Foo >: Null <: Object },
  )
}
