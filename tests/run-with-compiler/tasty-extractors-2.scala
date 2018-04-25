
import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.util.TastyPrinter

object Test {
  def main(args: Array[String]): Unit = {
    for (q <- quotes) {
      val (tree, ctx) = q.toTasty
      println(TastyPrinter.stringOf(tree)(ctx))
      println(TastyPrinter.stringOf(tree.tpe)(ctx))
      println()
    }
  }

  def quotes: List[Expr[_]] = List(
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
    '{ class Foo { final def a = 0 } },
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
    '{ val lambda: Int => Int = x => x },

  )
}
