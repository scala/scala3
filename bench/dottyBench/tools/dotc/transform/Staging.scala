package dottyBench.tools.dotc
package transform

import dottyBench.tools.dotc.ast.Trees._
import dottyBench.tools.dotc.ast.{TreeTypeMap, tpd, untpd}
import dottyBench.tools.dotc.core.Constants._
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Phases._
import dottyBench.tools.dotc.core.Decorators._
import dottyBench.tools.dotc.core.Flags._
import dottyBench.tools.dotc.core.quoted._
import dottyBench.tools.dotc.core.NameKinds._
import dottyBench.tools.dotc.core.StagingContext._
import dottyBench.tools.dotc.core.StdNames._
import dottyBench.tools.dotc.core.Symbols._
import dottyBench.tools.dotc.core.tasty.TreePickler.Hole
import dottyBench.tools.dotc.core.Types._
import dottyBench.tools.dotc.util.{SourceFile, SourcePosition}
import dottyBench.tools.dotc.transform.SymUtils._
import dottyBench.tools.dotc.transform.TreeMapWithStages._
import dottyBench.tools.dotc.typer.Implicits.SearchFailureType
import dottyBench.tools.dotc.typer.Inliner

import scala.collection.mutable
import dottyBench.tools.dotc.util.SourcePosition

import scala.annotation.constructorOnly

/** Checks that the Phase Consistency Principle (PCP) holds and heals types.
 *
 *  Type healing consists in transforming a phase inconsistent type `T` into `${ implicitly[Type[T]] }`.
 */
class Staging extends MacroTransform {
  import tpd._
  import Staging._

  override def phaseName: String = Staging.name

  override def allowsImplicitSearch: Boolean = true

  override def checkPostCondition(tree: Tree)(using Ctx, CState): Unit =
    if (currentPhase <= reifyQuotesPhase) {
      // Recheck that PCP holds but do not heal any inconsistent types as they should already have been heald
      tree match {
        case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
          val checker = new PCPCheckAndHeal(freshStagingContext) {
            override protected def tryHeal(sym: Symbol, tp: TypeRef, pos: SourcePosition)(using Ctx, CState): TypeRef = {
              def symStr =
                if (sym.is(ModuleClass)) sym.sourceModule.show
                else i"${sym.name}.this"
              val errMsg = s"\nin ${ctx.owner.fullName}"
              assert(
                ctx.owner.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot) ||
                (sym.isType && levelOf(sym) > 0),
                em"""access to $symStr from wrong staging level:
                    | - the definition is at level ${levelOf(sym)},
                    | - but the access is at level $level.$errMsg""")

              tp
            }
          }
          checker.transform(tree)
        case _ =>
      }

      tree.tpe match {
        case tpe @ TypeRef(prefix, _) if tpe.typeSymbol eq defn.QuotedType_splice =>
          // Type splices must have a know term ref, usually to an implicit argument
          // This is mostly intended to catch `quoted.Type[T]#splice` types which should just be `T`
          assert(prefix.isInstanceOf[TermRef] || prefix.isInstanceOf[ThisType], prefix)
        case _ =>
          // OK
      }
    }

  override def run(using Ctx, CState): Unit =
    if (ctx.compilationUnit.needsStaging) super.run(using freshStagingContext)

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Ctx, CState): tpd.Tree =
      new PCPCheckAndHeal(combinedContext).transform(tree)
  }
}


object Staging {
  val name: String = "staging"
}
