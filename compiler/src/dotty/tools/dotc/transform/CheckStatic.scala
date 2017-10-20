package dotty.tools.dotc
package transform

import core._
import Names._
import StdNames.nme
import Types._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees._
import Flags._
import Contexts.Context
import Symbols._
import Constants._
import Denotations._, SymDenotations._
import Decorators.StringInterpolators
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import scala.collection.mutable
import DenotTransformers._
import Names.Name
import NameOps._
import Decorators._
import TypeUtils._

/** A transformer that check that requirements of Static fields\methods are implemented:
  *  1. Only objects can have members annotated with `@static`
  *  2. The fields annotated with `@static` should preceed any non-`@static` fields.
  *     This ensures that we do not introduce surprises for users in initialization order.
  *  3. If a member `foo` of an `object C` is annotated with `@static`,
  *     the companion class `C` is not allowed to define term members with name `foo`.
  *  4. If a member `foo` of an `object C` is annotated with `@static`, the companion class `C`
  *     is not allowed to inherit classes that define a term member with name `foo`.
  *  5. Only `@static` methods and vals are supported in companions of traits.
  *     Java8 supports those, but not vars, and JavaScript does not have interfaces at all.
  *  6. `@static` Lazy vals are currently unsupported.
  */
class CheckStatic extends MiniPhaseTransform { thisTransformer =>
  import ast.tpd._

  override def phaseName = "checkStatic"

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val defns = tree.body.collect{case t: ValOrDefDef => t}
    var hadNonStaticField = false
    for(defn <- defns) {
      if (defn.symbol.hasAnnotation(ctx.definitions.ScalaStaticAnnot)) {
        if(!ctx.owner.is(Module)) {
          ctx.error("@static fields are only allowed inside objects", defn.pos)
        }

        if (defn.isInstanceOf[ValDef] && hadNonStaticField) {
          ctx.error("@static fields should preceed non-static ones", defn.pos)
        }

        val companion = ctx.owner.companionClass
        def clashes = companion.asClass.membersNamed(defn.name)

        if (!companion.exists) {
          ctx.error("object that contains @static members should have companion class", defn.pos)
        } else if (clashes.exists) {
          ctx.error("companion classes cannot define members with same name as @static member", defn.pos)
         } else if (defn.symbol.is(Flags.Mutable) && companion.is(Flags.Trait)) {
          ctx.error("Companions of traits cannot define mutable @static fields", defn.pos)
        } else if (defn.symbol.is(Flags.Lazy)) {
          ctx.error("Lazy @static fields are not supported", defn.pos)
        } else if (defn.symbol.allOverriddenSymbols.nonEmpty) {
          ctx.error("@static members cannot override or implement non-static ones", defn.pos)
        }
      } else hadNonStaticField = hadNonStaticField || defn.isInstanceOf[ValDef]

    }
    tree
  }

  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (tree.symbol.hasAnnotation(defn.ScalaStaticAnnot)) {
      val symbolWhitelist = tree.symbol.ownersIterator.flatMap(x => if (x.is(Flags.Module)) List(x, x.companionModule) else List(x)).toSet
      def isSafeQual(t: Tree): Boolean = { // follow the desugared paths created by typer
        t match {
          case t: This => true
          case t: Select => isSafeQual(t.qualifier) && symbolWhitelist.contains(t.symbol)
          case t: Ident => symbolWhitelist.contains(t.symbol)
          case t: Block => t.stats.forall(tpd.isPureExpr) && isSafeQual(t.expr)
        }
      }
      if (isSafeQual(tree.qualifier))
        ref(tree.symbol)
      else tree
    } else tree
  }
}
