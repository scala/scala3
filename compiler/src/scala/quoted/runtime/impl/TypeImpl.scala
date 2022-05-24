package scala.quoted
package runtime.impl

import dotty.tools.dotc.ast.tpd

/** Quoted type (or kind) `T` backed by a tree */
final class TypeImpl(val typeTree: tpd.Tree, val scope: Scope) extends Type[?] {
  override def equals(that: Any): Boolean = that match {
    case that: TypeImpl => typeTree ==
      // TastyTreeExpr are wrappers around trees, therefore they are equals if their trees are equal.
      // All scope should be equal unless two different runs of the compiler created the trees.
      that.typeTree && scope == that.scope
    case _ => false
  }

  override def hashCode(): Int = typeTree.hashCode()

  override def toString: String = "Type.of[...]"
}
