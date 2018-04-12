package dotty.tools.dotc.printing

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.untpd.{PackageDef, Template, TypeDef}
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.StdNames._

import scala.language.implicitConversions

class DecompilerPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  override protected def filterModTextAnnots(annots: List[untpd.Tree]): List[untpd.Tree] =
    annots.filter(_.tpe != defn.SourceFileAnnotType)

  override protected def blockToText[T >: Untyped](block: Block[T]): Text =
    block match {
      case Block(DefDef(_, _, _, _, Trees.If(cond, Trees.Block(body :: Nil, _), _)) :: Nil, y) if y.symbol.name == nme.WHILE_PREFIX =>
        keywordText("while") ~ " (" ~ toText(cond) ~ ")" ~ toText(body)
      case Block(DefDef(_, _, _, _, Trees.Block(body :: Nil, Trees.If(cond, _, _))) :: Nil, y) if y.symbol.name == nme.DO_WHILE_PREFIX =>
        keywordText("do") ~ toText(body) ~ keywordText("while") ~ " (" ~ toText(cond) ~ ")"
      case Block((meth @ DefDef(nme.ANON_FUN, _, _, _, _)) :: Nil, _: Closure[T]) =>
        withEnclosingDef(meth) {
          addVparamssText("", meth.vparamss) ~ " => " ~ toText(meth.rhs)
        }
      case _ =>
        super.blockToText(block)
    }

  override protected def packageDefText(tree: PackageDef): Text = {
    val stats = tree.stats.filter {
      case vdef: ValDef[_] => !vdef.symbol.is(Module)
      case _ => true
    }
    val statsText = stats match {
      case (pdef: PackageDef) :: Nil => toText(pdef)
      case _ => toTextGlobal(stats, "\n")
    }
    val bodyText =
      if (tree.pid.symbol.isEmptyPackage) statsText
      else if (currentPrecedence == TopLevelPrec) "\n" ~ statsText
      else " {" ~ statsText ~ "}"
    (keywordStr("package ") ~ toTextPackageId(tree.pid)).provided(!tree.symbol.isEmptyPackage) ~ bodyText
  }

  override protected def templateText(tree: TypeDef, impl: Template): Text = {
    val decl =
      if (!tree.mods.is(Module)) modText(tree.mods, keywordStr(if ((tree).mods is Trait) "trait" else "class"))
      else modText(tree.mods &~ (Final | Module), keywordStr("object"))
    decl ~~ typeText(nameIdText(tree)) ~ withEnclosingDef(tree) { toTextTemplate(impl) } ~ ""
  }

  override protected def toTextTemplate(impl: Template, ofNew: Boolean = false): Text = {
    val impl1 = impl.copy(parents = impl.parents.filterNot(_.symbol.maybeOwner == defn.ObjectClass))
    super.toTextTemplate(impl1, ofNew)
  }

  override protected def typeApplyText[T >: Untyped](tree: TypeApply[T]): Text = {
    if (tree.symbol eq defn.quoteMethod) "'"
    else if (tree.symbol eq defn.typeQuoteMethod) "'[" ~ toTextGlobal(tree.args, ", ") ~ "]"
    else super.typeApplyText(tree)
  }
}
