package scala.quoted
package runtime.impl

import dotty.tools.dotc.ast.tpd

/** An Expr backed by a tree. Only the current compiler trees are allowed.
 *
 *  These expressions are used for arguments of macros. They contain and actual tree
 *  from the program that is being expanded by the macro.
 *
 *  May contain references to code defined outside this Expr instance.
 */
final class ExprImpl(val tree: tpd.Tree, val scope: Scope) extends Expr[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: ExprImpl =>
      // Expr are wrappers around trees, therefore they are equals if their trees are equal.
      // All scope should be equal unless two different runs of the compiler created the trees.
      tree == that.tree && scope == that.scope
    case _ => false
  }

  override def hashCode(): Int = tree.hashCode()

  override def toString: String = "'{ ... }"
}
