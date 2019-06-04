package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd, untpd}
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.tasty.TreePickler.Hole
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Implicits.SearchFailureType
import dotty.tools.dotc.typer.Inliner

import scala.collection.mutable
import dotty.tools.dotc.util.SourcePosition

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

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    if (ctx.phase <= ctx.reifyQuotesPhase) {
      // Recheck that PCP holds but do not heal any inconsistent types as they should already have been heald
      tree match {
        case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
          val checker = new PCPCheckAndHeal(freshStagingContext) {
            override protected def tryHeal(sym: Symbol, tp: Type, pos: SourcePosition)(implicit ctx: Context): Option[tpd.Tree] = {
              def symStr =
                if (!tp.isInstanceOf[ThisType]) sym.show
                else if (sym.is(ModuleClass)) sym.sourceModule.show
                else i"${sym.name}.this"

              val errMsg = s"\nin ${ctx.owner.fullName}"
              assert(false,
                em"""access to $symStr from wrong staging level:
                    | - the definition is at level ${levelOf(sym).getOrElse(0)},
                    | - but the access is at level $level.$errMsg""")

              None
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
  }

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.needsStaging) super.run(freshStagingContext)

  protected def newTransformer(implicit ctx: Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree =
      new PCPCheckAndHeal(ctx).transform(tree)
  }

}

object Staging {
  val name: String = "staging"
}
