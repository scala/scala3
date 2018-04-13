
import scala.quoted._

import scala.tasty.constants.Constant
import scala.tasty.names
import scala.tasty.patterns
import scala.tasty.statements
import scala.tasty.terms
import scala.tasty.typetrees
import scala.tasty.types

import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

import scala.tasty.modifiers.Modifier

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
    '{ class Foo { @volatile var a = 0 } },
    '{ class Foo { final def a = 0 } }, // FIXME modifier not printed
    '{ class Foo { private[Foo] def a = 0 } },
    '{ class Foo { protected[Foo] def a = 0 } },
    '{ case class Foo() },
    '{ class Foo1(a: Int) },
    '{ class Foo2(val b: Int) },
    '{ class Foo3(a: Int = 5) },
    '{ class Foo4(a: Int)(b: Int) },
    '{ class Foo5(a: Int)(b: Int = a) },
    '{ class Foo6(a: Int)(b: a.type) },
//    '{ class Foo7(a: Int) { def this() = this(6) } },
    '{ class Foo8 { println(0) } },
    '{ class Foo10 { val a = 9 } },
    '{ class Foo11 { var a = 10 } },
    '{ class Foo12 { lazy val a = 11 } },
    '{ class Foo; class Bar extends Foo },
    '{ trait Foo2; class Bar extends Foo2 },
    '{ class Foo(i: Int); class Bar extends Foo(1) },
    '{ class Foo { type X = Int }; def f(a: Foo): a.X = ??? },
    '{ class Foo { type X }; def f(a: Foo { type X = Int }): a.X = ??? },

  )
}
