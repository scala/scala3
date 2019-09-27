import scala.quoted._, scala.deriving._
import scala.quoted.given

import scala.reflect.ClassTag

import Tuple.{ Head, Tail }
import scala.compiletime.{ erasedValue, summonFrom }


inline def mcr(given m: Mirror.ProductOf[Foo], m2: Mirror.ProductOf[Bar], m3: Mirror.ProductOf[Stuff.FooS], m4: Mirror.ProductOf[Stuff.BarS]): Any = ${mcrImpl(given 'm, 'm2, 'm3, 'm4)}
def mcrImpl(given m: Expr[Mirror.ProductOf[Foo]], m2: Expr[Mirror.ProductOf[Bar]], m3: Expr[Mirror.ProductOf[Stuff.FooS]], m4: Expr[Mirror.ProductOf[Stuff.BarS]])(given ctx: QuoteContext): Expr[Any] =
  val x = Foo(1, "foo")
  val y: Stuff = Stuff.FooS(10)
  val z: A = x
  val e1 = Expr(x)
  val e2 = Expr(y)
  val e3 = Expr(z)
  '{List($e1, $e2, $e3)}

sealed trait A
case class Foo(x: Int, y: String) extends A
case class Bar(a: String, b: Double) extends A

enum Stuff {
  case FooS(x: Int)
  case BarS(y: String)
}
