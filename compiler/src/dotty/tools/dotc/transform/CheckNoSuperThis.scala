package dotty.tools.dotc
package transform

import core.*
import MegaPhase.MiniPhase
import Contexts.*, Types.*, Symbols.*, SymDenotations.*, Flags.*
import ast.*
import Trees.*
import Decorators.*

import annotation.threadUnsafe

object CheckNoSuperThis:
  val name: String = "checkNoSuperThis"
  val description: String = "check that supercalls don't contain references to This"

/** Checks that super and this calls do not pass `this` as (part of) an argument. */
class CheckNoSuperThis extends MiniPhase:
  thisPhase =>
  import tpd._

  override def phaseName: String = CheckNoSuperThis.name

  override def description: String = CheckNoSuperThis.description

  override def runsAfterGroupsOf: Set[String] = Set(Constructors.name)

  override def transformDefDef(mdef: DefDef)(using Context): DefDef =
    if mdef.symbol.isClassConstructor then
      mdef.rhs match
        case Block(stats, _) => splitAtSuper(stats) match
          case (Apply(_, superArgs) :: _, _) =>
            val cls = mdef.symbol.owner
            def fail(t: Tree) =
              report.error(em"super constructor cannot be passed a self reference $t unless parameter is declared by-name", t.srcPos)
            for arg <- superArgs do
              arg.foreachSubTree {
                case t: This if t.symbol == cls =>
                  fail(t)
                case t: RefTree => t.tpe match
                  case tpe @ TermRef(prefix, _)
                  if (prefix == cls.thisType
                      || cls.is(Module)
                         && (prefix.termSymbol == cls.sourceModule || tpe.symbol == cls.sourceModule)
                    ) && !tpe.symbol.is(JavaStatic) => fail(t)
                  case _ =>
                case _ =>
              }
          case _ =>
        case _ =>
    mdef

end CheckNoSuperThis
