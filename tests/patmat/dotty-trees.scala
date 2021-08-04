abstract class Tree[-T >: Null]

case class TypeTree[-T >: Null]() extends Tree[T]

abstract class DerivedTypeTree() extends TypeTree[Null]

def foo(tree: Tree[Null]): Unit =
  tree match
  case _: DerivedTypeTree =>
  case TypeTree() =>
  case _ =>