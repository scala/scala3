import scala.quoted._, scala.deriving._
import scala.quoted.given

import scala.reflect.ClassTag

import Tuple.{ Head, Tail }
import scala.compiletime.{ erasedValue, summonFrom }


inline def mcr(given m: Mirror.ProductOf[Foo], m2: Mirror.ProductOf[Bar], m3: Mirror.ProductOf[Stuff.FooS], m4: Mirror.ProductOf[Stuff.BarS]): Any = ${mcrImpl(given 'm, 'm2, 'm3, 'm4)}
def mcrImpl(given m: Expr[Mirror.ProductOf[Foo]], m2: Expr[Mirror.ProductOf[Bar]], m3: Expr[Mirror.ProductOf[Stuff.FooS]], m4: Expr[Mirror.ProductOf[Stuff.BarS]])(given ctx: QuoteContext): Expr[Any] =
  val x: A = Foo(1, "foo")
  val y: Stuff = Stuff.FooS(10)
  Expr(y)

sealed trait A
case class Foo(x: Int, y: String) extends A
case class Bar(a: String, b: Double) extends A

enum Stuff {
  case FooS(x: Int)
  case BarS(y: String)
}

inline def summonInline[T] = summonFrom { case x: T => x }

inline def summonAll[T <: Tuple]: List[?] = inline erasedValue[T] match
  case _: Unit => Nil
  case _: (t *: ts) => summonInline[t] :: summonAll[ts]

inline given productLiftable[T <: Product: Type](given
    m : Mirror.ProductOf[T],
    em: Expr[Mirror.ProductOf[T]]): Liftable[T] = new Liftable[T] {
  def toExpr(x: T) =
    val genRepr = Tuple.fromProductTyped(x)
    val liftables = summonAll[Tuple.Map[m.MirroredElemTypes, Liftable]]
    val elemsWithLiftables = liftables.zip(genRepr.asInstanceOf[Product].productIterator.toList)
    val tupleOfExprs = elemsWithLiftables.map {
      case (l: Liftable[a], x) => l.toExpr(x.asInstanceOf[a])
    }
    val exprOfTuple = Expr.ofTuple(tupleOfExprs)
    '{$em.fromProduct($exprOfTuple.asInstanceOf[Product])}
}

inline given sumLiftable[T: Type](given m: Mirror.SumOf[T]): Liftable[T] = new Liftable[T] {
  def toExpr(x: T) =
    val liftables = summonAll[Tuple.Map[m.MirroredElemTypes, Liftable]]
    val tags = summonAll[Tuple.Map[m.MirroredElemTypes, ClassTag]]
    tags.zip(liftables).flatMap { case (t: ClassTag[a], l: Liftable[?]) =>
      t.unapply(x).map(xa => l.asInstanceOf[Liftable[a]].toExpr(xa)) }
      .head.asInstanceOf[Expr[T]]
}
