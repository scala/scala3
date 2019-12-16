package dotty.tools.dotc.tastyreflect

/** An Type backed by a tree */
final class TreeType[Tree](val typeTree: Tree, val scopeId: Int) extends scala.quoted.Type[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: TreeType[_] => typeTree ==
      // TastyTreeExpr are wrappers around trees, therfore they are equals if their trees are equal.
      // All scopeId should be equal unless two different runs of the compiler created the trees.
      that.typeTree && scopeId == that.scopeId
    case _ => false
  }
  override def hashCode: Int = typeTree.hashCode
  override def toString: String = s"Type(<tasty tree>)"
}
