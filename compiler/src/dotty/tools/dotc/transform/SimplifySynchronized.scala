package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

import scala.annotation.tailrec

// Turn non-inline methods whose body is contained within a `this.synchronized` call into synchronized methods
// Besides being more efficient, this also allows tail recursion in such methods
class SimplifySynchronized extends MiniPhase:
  override def phaseName: String = SimplifySynchronized.name
  override def description: String = SimplifySynchronized.description

  override def isEnabled(using Context): Boolean =
    super.isEnabled && ctx.platform.supportsSynchronizedMethods

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    def removeBoxedUnitReturns(body: Tree): Tree = body match
      case Ident(nme.UNIT) => Literal(Constant(()))
      case Block(stmt :: Nil, Ident(nme.UNIT)) => stmt
      case Block(Nil, e) => removeBoxedUnitReturns(e)
      case Block(stmts, e) => cpy.Block(body)(stmts, removeBoxedUnitReturns(e))
      case If(cond, thenp, elsep) => cpy.If(body)(cond, removeBoxedUnitReturns(thenp), removeBoxedUnitReturns(elsep))
      case t => t

    @tailrec
    def extractSynchronized(rhs: Tree): Option[Tree] = rhs match
      case Apply(TypeApply(Select(This(_), nme.synchronized_) | Ident(nme.synchronized_), _), synchronizedBody :: Nil) =>
        tree.symbol.denot.setFlag(Synchronized)
        val transformed = removeBoxedUnitReturns(synchronizedBody)
        Some(transformed)
      case Block(Nil, expr) =>
        extractSynchronized(expr)
      case Block(stmt :: Nil, Literal(Constant(()))) =>
        extractSynchronized(stmt)
      case _ =>
        None
    if !tree.symbol.is(Inline) && !tree.symbol.is(ExtensionMethod) && !tree.symbol.owner.is(Trait)
    then extractSynchronized(tree.rhs).map(body => cpy.DefDef(tree)(rhs = body)).getOrElse(tree)
    else tree

object SimplifySynchronized:
  val name: String = "simplifySynchronized"
  val description: String = "simplify synchronized methods where possible"