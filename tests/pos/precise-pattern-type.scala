object `precise-pattern-type` {
  class Type {
    def isType: Boolean = true
  }

  class Tree[-T >: Null] {
    def tpe: T @annotation.unchecked.uncheckedVariance = ???
  }

  case class Select[-T >: Null](qual: Tree[T]) extends Tree[T]

  def test[T <: Tree[Type]](tree: T) = tree match {
    case Select(q) =>
      q.tpe.isType
  }

  trait O {
    type ThisTree <: Tree[Type]
    val tree: ThisTree
    def test = tree match {
      case Select(q) => q.tpe.isType
      case tree1: Select[t] => (tree1 : Select[Type]).qual.tpe.isType
    }
  }

  trait OO {
    type ThisTree[T >: Null] <: Tree[T]
    def foo(t: ThisTree[Type]) = t match {
      case Select(q) => q.tpe.isType
    }
  }
}
