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

  val replace: mutable.Map[Tree, Tree] = mutable.Map()

  def clear(): Unit = replace.clear()

  def transformer(implicit ctx: Context): Tree => Tree = 
    tree => replace.getOrElse(tree, tree)


  def visitor(implicit ctx: Context): Tree => Unit =
    tree => tree match {

      case Block(stats, expr) => 
        def visitStatements(stats: List[Tree]): Unit =
          stats match {
            case s :: tail =>
              s match {
                case t: ValDef if !isVar(t.symbol) => 
                  if (isNotNull(t.rhs)) tail.foreach(flagAsNotNull(t.symbol, _))
                  else if (isNull(t.rhs)) tail.foreach(flagAsNull(t.symbol, _))

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
              nullChecksInExpr(expr) match {
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


  private def nullChecksInExpr(expr: Tree)(implicit ctx: Context): Option[Symbol] =
    expr match {
      case t @ Apply(Select(id: Ident, op), List(rhs)) if (t.symbol == defn.Object_eq || op == nme.EQ) && !isVar(id.symbol) && isNull(rhs) => Some(id.symbol)
      case t @ Apply(Select(lhs, op), List(id: Ident)) if (t.symbol == defn.Object_eq || op == nme.EQ) && !isVar(id.symbol) && isNull(lhs) => Some(id.symbol)
      case _ => None
    }

  private def findNullChecksFor(symbol: Symbol, tree: Tree)(implicit ctx: Context) = 
    tree.filterSubTrees(nullChecksInExpr(_) match { case Some(s) if (s == symbol) => true; case _ => false })

  private def flagAsNull(symbol: Symbol, tree: Tree)(implicit ctx: Context) = replace ++= findNullChecksFor(symbol, tree).map((_, Literal(Constant(true))))
  private def flagAsNotNull(symbol: Symbol, tree: Tree)(implicit ctx: Context) = replace ++= findNullChecksFor(symbol, tree).map((_, Literal(Constant(false))))

  private def isNotNull(tree: Tree)(implicit ctx: Context): Boolean =
    tree match {
      case Block(_, expr) => isNotNull(expr)
      case If(_, th, el) => isNotNull(th) && isNotNull(el)
      case t: Typed => isNotNull(t.expr)
      case t: Literal => t.const.tag != NullTag
      case _: This => true
      case _: New => true
      case t: Apply if t.symbol.isPrimaryConstructor => true
      case Apply(Select(New(_), _), _) => true
      case t => t.tpe.isNotNull
    }

  private def isNull(tree: Tree)(implicit ctx: Context): Boolean = 
    tree match {
      case Block(_, expr) => isNull(expr)
      case If(_, th, el) => isNull(th) && isNull(el)
      case t: Typed => isNull(t.expr)
      case t: Literal => t.const.tag == NullTag
      case EmptyTree => true
      case _ => false
    }

  private def isVar(s: Symbol)(implicit ctx: Context) = s.is(Mutable, Lazy) && !s.is(Method) && !s.owner.isClass
}
