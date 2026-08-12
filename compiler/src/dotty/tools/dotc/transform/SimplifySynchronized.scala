package dotty.tools.dotc.transform

import scala.annotation.tailrec
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.{Inline, Synchronized}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

class SimplifySynchronized extends MiniPhase:
  override def phaseName: String = SimplifySynchronized.name
  override def description: String = SimplifySynchronized.description

  // Turn non-inline methods whose body is contained within a `this.synchronized` call into synchronized methods
  // Besides being more efficient, this also allows tail recursion in such methods.
  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    @tailrec
    def extractSynchronized(rhs: Tree): Option[Tree] = rhs match
      case Apply(TypeApply(Ident(nme.synchronized_) | Select(This(_), nme.synchronized_), _), synchronizedBody :: Nil) =>
        Some(synchronizedBody)
      case Block(Nil, expr) =>
        extractSynchronized(expr)
      case _ =>
        None

    if canBeSynchronized(tree.symbol) then
      extractSynchronized(tree.rhs) match
        case Some(newRhs) =>
          tree.symbol.denot.setFlag(Synchronized)
          cpy.DefDef(tree)(rhs = newRhs)
        case None =>
          tree
    else
      tree
  }

  // However, this is only feasible for methods that are not inline
  private def canBeSynchronized(sym: Symbol)(using Context) =
    !sym.is(Inline)
      && (sym.owner match
            case cs: ClassSymbol => !cs.parentSyms.contains(defn.AnyValClass)
            case _ => false)

object SimplifySynchronized:
  val name: String = "simplifySynchronized"
  val description: String = "simplify synchronized blocks when they surround a method body"