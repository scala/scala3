package dotty.tools.dotc
package printing

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.untpd.{Tree, PackageDef, Template, TypeDef}
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Annotations.Annotation
import util.Lst; // import Lst.::
import util.Lst.{NIL, +:, toLst}

class DecompilerPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  override protected def dropAnnotForModText(sym: Symbol): Boolean =
    super.dropAnnotForModText(sym) || sym == defn.SourceFileAnnot

  override protected def blockToText[T >: Untyped](block: Block[T]): Text =
    block match {
      case Block(Lst(DefDef(_, _, _, _, Trees.If(cond, Trees.Block(Lst(body), _), _))), y) if y.symbol.name == nme.WHILE_PREFIX =>
        keywordText("while") ~ " (" ~ toText(cond) ~ ")" ~ toText(body)
      case Block(Lst(DefDef(_, _, _, _, Trees.Block(Lst(body), Trees.If(cond, _, _)))), y) if y.symbol.name == nme.DO_WHILE_PREFIX =>
        keywordText("do") ~ toText(body) ~ keywordText("while") ~ " (" ~ toText(cond) ~ ")"
      case Block(Lst(meth @ DefDef(nme.ANON_FUN, _, _, _, _)), _: Closure[T]) =>
        withEnclosingDef(meth) {
          addVparamssText("", meth.vparamss) ~ " => " ~ toText(meth.rhs)
        }
      case _ =>
        super.blockToText(block)
    }

  override protected def packageDefText(tree: PackageDef): Text = {
    val stats: Lst[Tree] = tree.stats.filter {
      case vdef: ValDef[?] => !vdef.symbol.is(Module)
      case _ => true
    }
    val statsText = stats match {
      case Lst(pdef: PackageDef) => toText(pdef)
      case _ => Fluid(toTextGlobal(stats, "\n") :: Nil)
    }
    val bodyText =
      if (tree.pid.symbol.isEmptyPackage) statsText
      else if (currentPrecedence == TopLevelPrec) "\n" ~ statsText
      else " {" ~ statsText ~ "}"
    (keywordStr("package ") ~ toTextPackageId(tree.pid)).provided(!tree.symbol.isEmptyPackage) ~ bodyText
  }

  override protected def templateText(tree: TypeDef, impl: Template): Text = {
    val decl =
      if (!tree.mods.is(Module)) modText(tree.mods, tree.symbol, keywordStr(if (tree.mods.is(Trait)) "trait" else "class"), isType = true)
      else modText(tree.mods, tree.symbol, keywordStr("object"), isType = false)
    decl ~~ typeText(nameIdText(tree)) ~ withEnclosingDef(tree) { toTextTemplate(impl) } ~ ""
  }

  override protected def toTextTemplate(impl: Template, ofNew: Boolean = false): Text = {
    def isSynthetic(parent: Tree): Boolean = {
      val sym = parent.symbol
      sym.maybeOwner == defn.ObjectClass ||
      (sym == defn.ProductClass && impl.symbol.owner.is(Case)) ||
      (sym == defn.SerializableClass && impl.symbol.owner.is(Case))
    }
    val parents = impl.parents.filterNot(isSynthetic)
    val body = impl.body.filterNot(_.symbol.is(ParamAccessor))

    // We don't print self type and constructor for objects
    val isObject = impl.constr.symbol.owner.is(Module)
    if (isObject) {
      val parentsText = keywordText(" extends") ~~ Text(parents.map(constrText), keywordStr(" with "))
      val bodyText = " {" ~~ toTextGlobal(impl.body, "\n") ~ "}"
      parentsText.provided(parents.nonEmpty) ~ bodyText
    }
    else super.toTextTemplate(untpd.cpy.Template(impl)(parents = parents, body = body), ofNew)
  }

  override protected def typeApplyText[T >: Untyped](tree: TypeApply[T]): Text =
    if (tree.symbol eq defn.InternalQuoted_exprQuote) "'"
    else super.typeApplyText(tree)
}
