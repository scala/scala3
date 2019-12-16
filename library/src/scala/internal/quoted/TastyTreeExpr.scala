package scala.internal.quoted

import scala.quoted.Expr

/** An Expr backed by a tree. Only the current compiler trees are allowed.
  *
  *  These expressions are used for arguments of macros. They contain and actual tree
  *  from the program that is being expanded by the macro.
  *
  *  May contain references to code defined outside this TastyTreeExpr instance.
  */
final class TastyTreeExpr[Tree](val tree: Tree, val scopeId: Int) extends Expr[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: TastyTreeExpr[_] =>
      // TastyTreeExpr are wrappers around trees, therfore they are equals if their trees are equal.
      // All scopeId should be equal unless two different runs of the compiler created the trees.
      tree == that.tree && scopeId == that.scopeId
    case _ => false
  }
  override def hashCode: Int = tree.hashCode
  override def toString: String = s"Expr(<tasty tree>)"
}
