package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.MegaPhase._
import Flags._
import Contexts._
import Symbols._
import dotty.tools.dotc.ast.tpd
import Decorators._
import reporting._

import dotty.tools.dotc.transform.SymUtils._

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

  override def description: String = CheckStatic.description

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    val defns = tree.body.collect{case t: ValOrDefDef => t}
    var hadNonStaticField = false
    for (defn <- defns)
      if (defn.symbol.isScalaStatic) {
        if (!ctx.owner.isStaticOwner)
          report.error(StaticFieldsOnlyAllowedInObjects(defn.symbol), defn.srcPos)
          defn.symbol.resetFlag(JavaStatic)

        if (defn.isInstanceOf[ValDef] && hadNonStaticField)
          report.error(StaticFieldsShouldPrecedeNonStatic(defn.symbol, defns), defn.srcPos)

        val companion = ctx.owner.companionClass
        def clashes = companion.asClass.membersNamed(defn.name)

        if (!companion.exists)
          report.error(MissingCompanionForStatic(defn.symbol), defn.srcPos)
        else if (clashes.exists)
          report.error(MemberWithSameNameAsStatic(), defn.srcPos)
        else if (defn.symbol.is(Flags.Mutable) && companion.is(Flags.Trait))
          report.error(TraitCompanionWithMutableStatic(), defn.srcPos)
        else if (defn.symbol.is(Flags.Lazy))
          report.error(LazyStaticField(), defn.srcPos)
        else if (defn.symbol.allOverriddenSymbols.nonEmpty)
          report.error(StaticOverridingNonStaticMembers(), defn.srcPos)
      }
      else hadNonStaticField = hadNonStaticField || defn.isInstanceOf[ValDef]

    tree
  }
}

object CheckStatic {
  val name: String = "checkStatic"
  val description: String = "check restrictions that apply to @static members"
}
