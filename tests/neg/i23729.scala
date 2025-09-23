
trait Collection[Self, Element]:
  type Index
  extension (self: Self)
    def start: Index

sealed trait Tree[+T]
object Tree:
  case object Empty extends Tree[Nothing]
  case class Node[+T](value: T, lhs: Tree[T], rhs: Tree[T]) extends Tree[T]

enum Direction:
  case Left, Right, Here
given [T]: Collection[Tree[T], T] with
  type Index = List[Direction]
  extension (self: Tree[T])
    def start: List[Direction] = match self // error syntax
      case Empty => Nil // error poor recovery
      case Node(_, l, _) => l.start :+ Left // error poor recovery
