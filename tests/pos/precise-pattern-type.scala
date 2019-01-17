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
}
