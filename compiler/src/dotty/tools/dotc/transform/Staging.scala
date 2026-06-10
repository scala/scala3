package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.util.SrcPos

import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.staging.CrossStageSafety
import dotty.tools.dotc.staging.HealType

/** Checks that staging level consistency holds and heals types used in higher levels.
 *
 *  See `CrossStageSafety`
 */
class Staging extends MacroTransform {
  import tpd.*

  override def phaseName: String = Staging.name

  override def description: String = Staging.description

  override def runsAfter: Set[String] = Set(Inlining.name)

  override def allowsImplicitSearch: Boolean = true

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    if (ctx.phase <= stagingPhase) {
      // Recheck that staging level consistency holds but do not heal any inconsistent types as they should already have been heald
      tree match {
        case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
          val checker = new CrossStageSafety {
            override protected def healType(pos: SrcPos)(tpe: Type)(using Context) = new HealType(pos) {
              override protected def tryHeal(tp: TypeRef): TypeRef = {
                val sym = tp.symbol
                def symStr =
                  if (sym.is(ModuleClass)) sym.sourceModule.show
                  else i"${sym.name}.this"
                val errMsg = s"\nin ${ctx.owner.fullName}"
                assert(
                  ctx.owner.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) ||
                  (sym.isType && levelOf(sym) > 0),
                  em"""access to $symStr from wrong staging level:
                      | - the definition is at level ${levelOf(sym)},
                      | - but the access is at level $level.$errMsg""")

                tp
              }
            }.apply(tpe)
          }
          checker.transform(tree)
        case _ =>
      }
    }
    if !Inlines.inInlineMethod then
      tree match {
        case tree: RefTree =>
          assert(level != 0 || tree.symbol != defn.QuotedTypeModule_of,
            "scala.quoted.Type.of at level 0 should have been replaced with Quote AST in staging phase")
        case _ =>
      }

      tree.tpe match {
        case tpe @ TypeRef(prefix, _) if tpe.typeSymbol.isTypeSplice =>
          // Type splices must have a know term ref, usually to an implicit argument
          // This is mostly intended to catch `quoted.Type[T]#splice` types which should just be `T`
          assert(prefix.isInstanceOf[TermRef] || prefix.isInstanceOf[ThisType], prefix)
        case _ =>
          // OK
      }
  end checkPostCondition

  override protected def run(using Context): Unit =
    if (ctx.compilationUnit.needsStaging) {
      // Assume no level-0 quote survives until the walk below encounters one;
      // `CrossStageSafety` records each survivor so that `Splicing`, which runs
      // on this exact tree just after this phase, can skip survivor-free units.
      ctx.compilationUnit.hasLevel0Quotes = false
      // Most `needsStaging` units carry no staged quote by this point: the quotes that
      // set the flag were consumed by macro expansion (see `stagedQuoteSurvivors`). The
      // `CrossStageSafety` walk is the identity outside quote/splice scope, so it can be
      // skipped for those units. Units with macro annotations are walked unconditionally
      // since their expansions may insert quotes without re-typing them through
      // `InlineTyper`.
      if (ctx.compilationUnit.stagedQuoteSurvivors || ctx.compilationUnit.hasMacroAnnotations)
        super.run
    }

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      (new CrossStageSafety).transform(tree)
  }
}


object Staging {
  val name: String = "staging"
  val description: String = "check staging levels and heal staged types"
}
