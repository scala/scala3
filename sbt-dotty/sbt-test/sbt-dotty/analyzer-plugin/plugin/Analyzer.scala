package analyzer

import scala.language.implicitConversions

import dotty.tools.dotc._
import core._
import Contexts.Context
import plugins._
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators._
import Symbols.Symbol
import Constants.Constant
import transform.{Pickler, Staging}

class InitChecker extends PluginPhase with StandardPlugin {
  import tpd._

  val name: String = "initChecker"
  override val description: String = "checks that under -Yretain-trees we may get tree for all symbols"

  val phaseName = name

  override val runsAfter = Set(Staging.name)
  override val runsBefore = Set(Pickler.name)

  def init(options: List[String]): List[PluginPhase] = this :: Nil

  private def checkDef(tree: Tree)(implicit ctx: Context): Tree = {
    val span = tree.symbol.defTree.span
    if (!(span.exists && span.end > span.start))
      ctx.error("cannot get tree for " + tree.symbol.show, tree.sourcePos)
    tree
  }

  private def checkRef(tree: Tree)(implicit ctx: Context): Tree = {
    val helloPkgSym = ctx.requiredPackage("hello")
    val libPkgSym = ctx.requiredPackage("lib")
    val enclosingPkg = tree.symbol.enclosingPackageClass

    if (enclosingPkg == helloPkgSym) {  // source code
      checkDef(tree)
    }
    else if (enclosingPkg == libPkgSym) { // tasty from library
      checkDef(tree)
      // check that all sub-definitions have trees set properly
      transformAllDeep(tree.symbol.defTree)
    }

    tree
  }

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = checkDef(tree)

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = checkDef(tree)

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = checkDef(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree = checkRef(tree)

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree = checkRef(tree)
}