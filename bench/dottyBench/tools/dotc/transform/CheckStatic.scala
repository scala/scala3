package dottyBench.tools.dotc
package transform

import core._
import dottyBench.tools.dotc.transform.MegaPhase._
import Flags._
import Contexts._
import Symbols._
import dottyBench.tools.dotc.ast.tpd
import Decorators._
import reporting._

import dottyBench.tools.dotc.transform.SymUtils._

/** A transformer that check that requirements of Static fields\methods are implemented:
  *  1. Only objects can have members annotated with `@static`
  *  2. The fields annotated with `@static` should precede any non-`@static` fields.
  *     This ensures that we do not introduce surprises for users in initialization order.
  *  3. If a member `foo` of an `object C` is annotated with `@static`,
  *     the companion class `C` is not allowed to define term members with name `foo`.
  *  4. If a member `foo` of an `object C` is annotated with `@static`, the companion class `C`
  *     is not allowed to inherit classes that define a term member with name `foo`.
  *  5. Only `@static` methods and vals are supported in companions of traits.
  *     Java8 supports those, but not vars, and JavaScript does not have interfaces at all.
  *  6. `@static` Lazy vals are currently unsupported.
  */
class CheckStatic extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = CheckStatic.name

  override def transformTemplate(tree: tpd.Template)(using Ctx, CState): tpd.Tree = {
    val defns = tree.body.collect{case t: ValOrDefDef => t}
    var hadNonStaticField = false
    for (defn <- defns)
      if (defn.symbol.isScalaStatic) {
        if (!ctx.owner.is(Module))
          report.error(StaticFieldsOnlyAllowedInObjects(defn.symbol), defn.sourcePos)

        if (defn.isInstanceOf[ValDef] && hadNonStaticField)
          report.error(StaticFieldsShouldPrecedeNonStatic(defn.symbol, defns), defn.sourcePos)

        val companion = ctx.owner.companionClass
        def clashes = companion.asClass.membersNamed(defn.name)

        if (!companion.exists)
          report.error(MissingCompanionForStatic(defn.symbol), defn.sourcePos)
        else if (clashes.exists)
          report.error(MemberWithSameNameAsStatic(), defn.sourcePos)
        else if (defn.symbol.is(Flags.Mutable) && companion.is(Flags.Trait))
          report.error(TraitCompanionWithMutableStatic(), defn.sourcePos)
        else if (defn.symbol.is(Flags.Lazy))
          report.error(LazyStaticField(), defn.sourcePos)
        else if (defn.symbol.allOverriddenSymbols.nonEmpty)
          report.error(StaticOverridingNonStaticMembers(), defn.sourcePos)
      }
      else hadNonStaticField = hadNonStaticField || defn.isInstanceOf[ValDef]

    tree
  }

  override def transformSelect(tree: tpd.Select)(using Ctx, CState): tpd.Tree =
    if (tree.symbol.hasAnnotation(defn.ScalaStaticAnnot)) {
      val symbolWhitelist = tree.symbol.ownersIterator.flatMap(x => if (x.is(Flags.Module)) List(x, x.companionModule) else List(x)).toSet
      def isSafeQual(t: Tree): Boolean = // follow the desugared paths created by typer
        t match {
          case t: This => true
          case t: Select => isSafeQual(t.qualifier) && symbolWhitelist.contains(t.symbol)
          case t: Ident => symbolWhitelist.contains(t.symbol)
          case t: Block => t.stats.forall(tpd.isPureExpr) && isSafeQual(t.expr)
        }
      if (isSafeQual(tree.qualifier))
        ref(tree.symbol)
      else tree
    }
    else tree
}

object CheckStatic {
  val name: String = "checkStatic"
}
