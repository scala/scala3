import collection.immutable.TreeSet
import collection.immutable.HashSet
import scala.quoted.*


transparent inline def f1[T](): Set[T] = ${ f1Impl[T] } : Set[T]

def f1Impl[T: Type](using Quotes): Expr[Set[T]] = {
  Expr.summon[Ordering[T]] match {
    case Some(ord) => '{ new TreeSet[T]()($ord) }
    case _ => '{ new HashSet[T] }
  }
}

class A
class B

transparent inline def g = ${ gImpl } : Unit

def gImpl(using Quotes): Expr[Unit] = {
  if (Expr.summon[A].isDefined) '{ println("A") }
  else if (Expr.summon[B].isDefined) '{ println("B") }
  else throw new MatchError("")
}
