import collection.immutable.TreeSet
import collection.immutable.HashSet
import scala.quoted._
import scala.quoted.matching._

inline def f1[T]() = ${ f1Impl[T] }

def f1Impl[T: Type] given QuoteContext = {
  searchImplicitExpr[Ordering[T]] match {
    case Some(ord) => '{ new TreeSet[T]()($ord) }
    case _ => '{ new HashSet[T] }
  }
}

class A
class B

inline def g = ${ gImpl }

def gImpl given QuoteContext = {
  if (searchImplicitExpr[A].isDefined) '{ println("A") }
  else if (searchImplicitExpr[B].isDefined) '{ println("B") }
  else throw new MatchError("")
}
