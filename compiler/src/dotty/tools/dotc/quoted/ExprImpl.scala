package dotty.tools.dotc.quoted

import scala.quoted._

import dotty.tools.dotc.ast.tpd

/** An Expr backed by a tree. Only the current compiler trees are allowed.
 *
 *  These expressions are used for arguments of macros. They contain and actual tree
 *  from the program that is being expanded by the macro.
 *
 *  May contain references to code defined outside this Expr instance.
 */
final class ExprImpl(val tree: tpd.Tree, val scopeId: Int) extends scala.quoted.internal.Expr[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: ExprImpl =>
      // Expr are wrappers around trees, therefore they are equals if their trees are equal.
      // All scopeId should be equal unless two different runs of the compiler created the trees.
      tree == that.tree && scopeId == that.scopeId
    case _ => false
  }

  def checkScopeId(expectedScopeId: Int): Unit =
    if expectedScopeId != scopeId then
      throw new ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  override def hashCode: Int = tree.hashCode
  override def toString: String = "'{ ... }"
}
