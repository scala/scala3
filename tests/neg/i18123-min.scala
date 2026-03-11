trait P[+T]

trait Repeater[-T, R]
object Repeater:
  given generic[T]: Repeater[T, Seq[T]] = ???

extension [T](parse0: P[T])
  def |[V >: T](other: P[V]): P[V] = ???

extension [T](parse0: => P[T])
  def rep[V](min: Int = 0)(using Repeater[T, V]): P[V] = ???

sealed trait RegexTree
class CharClassIntersection extends RegexTree

def classItem: P[RegexTree] = ???
def charClassIntersection: P[CharClassIntersection] = ???

def test =
  charClassIntersection.rep() | classItem.rep() // error
