package dotty.tools.dotc
package transform

import core.*
import MegaPhase.MiniPhase
import Contexts.*, Types.*, Symbols.*, SymDenotations.*, Flags.*
import ast.*
import Decorators.*
import StdNames.*

object CheckLoopingImplicits:
  val name: String = "checkLoopingImplicits"
  val description: String = "check that implicit defs do not call themselves in an infinite loop"

/** Checks that some definitions do not call themselves in an infinite loop
 *  This is an incomplete check, designed to catch some likely bugs instead
 *  of being exhaustive. The situations where infinite loops are diagnosed are
 *   1. A given method should not directly call itself
 *   2. An apply method in a given object should not directly call itself
 *   3. A lazy val should not directly force itself
 *   4. An extension method should not directly call itself
 *
 *  In all these cases, there are some situations which would not lead to
 *  an infinite loop at runtime. For instance, the call could go at runtime to an
 *  overriding version of the method or val which breaks the loop. That's why
 *  this phase only issues warnings, not errors, and also why we restrict
 *  checks to the 4 cases above, where a recursion is somewhat hidden.
 *  There are also other more complicated calling patterns that could also
 *  be diagnosed as loops with more effort. This could be improved in the future.
 */
class CheckLoopingImplicits extends MiniPhase:
  thisPhase =>
  import tpd._

  override def phaseName: String = CheckLoopingImplicits.name

  override def description: String = CheckLoopingImplicits.description

  override def transformValDef(mdef: ValDef)(using Context): Tree =
    transform(mdef)

  override def transformDefDef(mdef: DefDef)(using Context): Tree =
    transform(mdef)

  def transform(mdef: ValOrDefDef)(using Context): Tree =
    val sym = mdef.symbol

    def checkNotSelfRef(t: RefTree) =
      if t.symbol eq sym then
          report.warning(
              em"""Infinite loop in function body
                  |${mdef.rhs}""",
              mdef.rhs.srcPos
            )

    def checkNotLooping(t: Tree): Unit = t match
      case t: Ident =>
        checkNotSelfRef(t)
      case t @ Select(qual, _) =>
        checkNotSelfRef(t)
        checkNotLooping(qual)
      case Apply(fn, args) =>
        checkNotLooping(fn)
        fn.tpe.widen match
          case mt: MethodType
               // Boolean && and || aren't defined with by-name parameters
               // and therefore their type isn't an ExprType, so we exempt them by symbol name
               if t.symbol != defn.Boolean_&& && t.symbol != defn.Boolean_|| =>
            args.lazyZip(mt.paramInfos).foreach { (arg, pinfo) =>
              if !pinfo.isInstanceOf[ExprType] then checkNotLooping(arg)
            }
          case _ =>
      case TypeApply(fn, _) =>
        checkNotLooping(fn)
      case Block(stats, expr) =>
        stats.foreach(checkNotLooping)
        checkNotLooping(expr)
      case Inlined(_, bindings, expr) =>
        bindings.foreach(checkNotLooping)
        checkNotLooping(expr)
      case Typed(expr, _) =>
        checkNotLooping(expr)
      case Assign(lhs, rhs) =>
        checkNotLooping(lhs)
        checkNotLooping(rhs)
      case If(cond, _, _) =>
        checkNotLooping(cond)
      case Match(selector, _) =>
        checkNotLooping(selector)
      case Labeled(_, expr) =>
        checkNotLooping(expr)
      case Return(expr, _) =>
        checkNotLooping(expr)
      case WhileDo(cond, _) =>
        checkNotLooping(cond)
      case Try(block, _, finalizer) =>
        checkNotLooping(block)
        checkNotLooping(finalizer)
      case SeqLiteral(elems, _) =>
        elems.foreach(checkNotLooping)
      case t: ValDef if !t.symbol.is(Lazy) =>
        checkNotLooping(t.rhs)
      case _ =>

    if sym.isOneOf(GivenOrImplicit | Lazy | ExtensionMethod)
      || sym.name == nme.apply && sym.owner.is(Module) && sym.owner.sourceModule.isOneOf(GivenOrImplicit)
    then
      checkNotLooping(mdef.rhs)
    mdef
  end transform
end CheckLoopingImplicits
