import collection.immutable.TreeSet
import collection.immutable.HashSet
import scala.quoted.*


inline def f1[T]() = ${ f1Impl[T] }

def f1Impl[T: Type](using Quotes) = {
  Expr.summon[Ordering[T]] match {
    case Some(ord) => '{ new TreeSet[T]()($ord) }
    case _ => '{ new HashSet[T] }
  }
}

class A
class B

inline def g = ${ gImpl }

def gImpl(using Quotes) = {
  if (Expr.summon[A].isDefined) '{ println("A") }
  else if (Expr.summon[B].isDefined) '{ println("B") }
  else throw new MatchError("")
}
