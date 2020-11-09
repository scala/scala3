package scala.quoted.internal

import scala.quoted._

/** An Expr backed by a tree. Only the current compiler trees are allowed.
 *
 *  These expressions are used for arguments of macros. They contain and actual tree
 *  from the program that is being expanded by the macro.
 *
 *  May contain references to code defined outside this Expr instance.
 */
final class Expr[Tree](val tree: Tree, val scopeId: Int) extends scala.quoted.Expr[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: Expr[_] =>
      // Expr are wrappers around trees, therefore they are equals if their trees are equal.
      // All scopeId should be equal unless two different runs of the compiler created the trees.
      tree == that.tree && scopeId == that.scopeId
    case _ => false
  }

  def unseal(using qctx: QuoteContext): qctx.reflect.Term =
    checkScopeId(qctx.hashCode)
    tree.asInstanceOf[qctx.reflect.Term]

  def checkScopeId(expectedScopeId: Int): Unit =
    if expectedScopeId != scopeId then
      throw new Exception("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  override def hashCode: Int = tree.hashCode
  override def toString: String = "'{ ... }"
}
