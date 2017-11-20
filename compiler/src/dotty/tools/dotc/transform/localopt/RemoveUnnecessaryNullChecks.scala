package dotty.tools.dotc
package transform.localopt

import core.Constants.{Constant, NullTag}
import core.Contexts.Context
import core.Symbols._
import core.Types._
import core.Flags._
import core.Names._
import core.StdNames._
import ast.Trees._
import scala.collection.mutable

/** Eliminate unnecessary null checks
 *
 *  - (this)  cannot be null
 *  - (new C) cannot be null
 *  - literal is either null itself or non null
 *  - fallsback to `tpe.isNotNull`, which will eventually be true for non nullable types.
 *  - in (a.call; a == null), the first call throws a NPE if a is null; the test can be removed.
 *
 *  
 *  @author DarkDimius, Jvican, OlivierBlanvillain, gan74
 */
class RemoveUnnecessaryNullChecks extends Optimisation {
  import ast.tpd._

  final class IdentityHashMap[K <: AnyRef, E]() extends mutable.HashMap[K, E] with mutable.MapLike[K, E, IdentityHashMap[K, E]] {
    override protected def elemEquals(a: K, b: K): Boolean = a eq b
    override protected def elemHashCode(k: K) = System.identityHashCode(k)
    override def empty = new IdentityHashMap[K, E]
  }


  // tree to replace
  val replace: IdentityHashMap[Tree, Tree] = new IdentityHashMap[Tree, Tree]
  // trees that are duplicated and can't be replaced safely
  val duplicate: IdentityHashMap[Tree, Int] = new IdentityHashMap[Tree, Int]

  def clear(): Unit = {
    replace.clear()
    duplicate.clear()
  }

  def transformer(implicit ctx: Context): Tree => Tree = 
    tree => if (duplicate.getOrElse(tree, 1) == 1) replace.getOrElse(tree, tree) else tree


  def visitor(implicit ctx: Context): Tree => Unit =
    tree => {
      duplicate(tree) = duplicate.getOrElse(tree, 0) + 1

      tree match {

        case Block(stats, expr) => 
          def visitStatements(stats: List[Tree]): Unit =
            stats match {
              case s :: tail =>
                s match {
                  case t: ValDef if !isVar(t.symbol) => 
                    if (isNeverNull(t.rhs)) tail.foreach(flagAsNotNull(t.symbol, _))
                    else if (isAlwaysNull(t.rhs)) tail.foreach(flagAsNull(t.symbol, _))

                  case Apply(Select(id: Ident, _), _) if !isVar(id.symbol) => 
                    tail.foreach(flagAsNotNull(id.symbol, _))

                  case _ =>
                }
                visitStatements(tail)
              case Nil => 
            }

          visitStatements(stats :+ expr)

        case If(cond, th, el) => 
          def visitBranches(cond: Tree, th: Tree, el: Tree): Unit = {
            cond match {
              case cond @ Select(expr, _) if cond.symbol == defn.Boolean_! =>
                visitBranches(expr, el, th) 
              case cond @ Apply(Select(lhs, _), List(rhs)) if cond.symbol == defn.Boolean_&& =>
                visitBranches(lhs, th, el)
                visitBranches(rhs, th, el)
              case expr =>
                nullCheckInExpr(expr) match {
                  case Some(symbol) => 
                    flagAsNull(symbol, th)
                    flagAsNotNull(symbol, el)
                  case None =>

                }
            }
          }
         
          visitBranches(cond, th, el)

        case _ =>
      } 
    }

  // return nullchecked symbol in expr (if any)
  private def nullCheckInExpr(expr: Tree)(implicit ctx: Context): Option[Symbol] =
    expr match {
      case t @ Apply(Select(id: Ident, op), List(rhs)) if (t.symbol == defn.Object_eq || t.symbol == defn.Any_==) && !isVar(id.symbol) && isAlwaysNull(rhs) => Some(id.symbol)
      case t @ Apply(Select(lhs, op), List(id: Ident)) if (t.symbol == defn.Object_eq || t.symbol == defn.Any_==) && !isVar(id.symbol) && isAlwaysNull(lhs) => Some(id.symbol)
      case _ => None
    }

  // return all sub-trees of tree containing a nullcheck for symbol
  private def findNullChecksFor(symbol: Symbol, tree: Tree)(implicit ctx: Context): List[Tree] =
    tree.filterSubTrees(nullCheckInExpr(_).contains(symbol))


  // fill replace using findNullCheckFor
  private def flagAsNull(symbol: Symbol, tree: Tree)(implicit ctx: Context): Unit =
    replace ++= findNullChecksFor(symbol, tree).map((_, Literal(Constant(true))))

  private def flagAsNotNull(symbol: Symbol, tree: Tree)(implicit ctx: Context): Unit = 
    replace ++= findNullChecksFor(symbol, tree).map((_, Literal(Constant(false))))

  private def isNeverNull(tree: Tree)(implicit ctx: Context): Boolean =
    tree match {
      case Block(_, expr) => isNeverNull(expr)
      case If(_, th, el) => isNeverNull(th) && isNeverNull(el)
      case t: Typed => isNeverNull(t.expr)
      case t: Literal => t.const.tag != NullTag
      case _: This => true
      case _: New => true
      case t: Apply if t.symbol.isPrimaryConstructor => true
      case Apply(Select(New(_), _), _) => true
      case t => t.tpe.isNotNull
    }

  private def isAlwaysNull(tree: Tree)(implicit ctx: Context): Boolean = 
    tree match {
      case Block(_, expr) => isAlwaysNull(expr)
      case If(_, th, el) => isAlwaysNull(th) && isAlwaysNull(el)
      case t: Typed => isAlwaysNull(t.expr)
      case t: Literal => t.const.tag == NullTag
      case EmptyTree => true
      case _ => false
    }

  private def isVar(s: Symbol)(implicit ctx: Context): Boolean = s.is(Mutable | Lazy)
}
