package dotty.tools.dotc.printing

import dotty.tools.dotc.ast.Trees.{Closure, DefDef, Untyped, ValDef}
import dotty.tools.dotc.ast.untpd.{PackageDef, Template, TypeDef}
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._

import scala.language.implicitConversions

class DecompilerPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  override protected def filterModTextAnnots(annots: List[untpd.Tree]): List[untpd.Tree] =
    annots.filter(_.tpe != defn.SourceFileAnnotType)

  override protected def blockText[T >: Untyped](trees: List[Trees.Tree[T]]): Text = {
    super.blockText(trees.filterNot(_.isInstanceOf[Closure[_]]))
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
      if (currentPrecedence == TopLevelPrec) "\n" ~ statsText else " {" ~ statsText ~ "}"
    keywordStr("package ") ~ toTextPackageId(tree.pid) ~ bodyText
  }

  override protected def templateText(tree: TypeDef, impl: Template): Text = {
    val decl =
      if (!modifiers(tree).is(Module)) modText(modifiers(tree), keywordStr(if (modifiers(tree) is Trait) "trait" else "class"))
      else modText(modifiers(tree) &~ (Final | Module), keywordStr("object"))
    decl ~~ typeText(nameIdText(tree)) ~ withEnclosingDef(tree) { toTextTemplate(impl) } ~ ""
  }

  override protected def defDefToText[T >: Untyped](tree: DefDef[T]): Text = {
    dclTextOr(tree) {
      val printLambda = tree.symbol.isAnonymousFunction
      val prefix = modText(modifiers(tree), keywordStr("def")) ~~ valDefText(nameIdText(tree)) provided (!printLambda)
      withEnclosingDef(tree) {
        addVparamssText(prefix ~ tparamsText(tree.tparams), tree.vparamss) ~ optAscription(tree.tpt).provided(!printLambda) ~
          optText(tree.rhs)((if (printLambda) " => " else " = ") ~ _)
      }
    }
  }
}
