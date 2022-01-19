package analyzer

import scala.language.implicitConversions

import dotty.tools.dotc._
import core._
import Symbols._
import Contexts.Context
import plugins._
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators._
import Symbols.Symbol
import Constants.Constant
import Types._
import transform.ProtectedAccessors

class InitPlugin extends StandardPlugin {
  import tpd._
  val name: String = "initPlugin"
  override val description: String = "checks that under -Yretain-trees we may get tree for all symbols"

  def init(options: List[String]): List[PluginPhase] =
    (new SetDefTree) :: (new InitChecker) :: Nil
}

class InitChecker extends PluginPhase {
  import tpd._

  val phaseName = "symbolTreeChecker"

  override val runsAfter = Set(SetDefTree.name)
  override val runsBefore = Set(ProtectedAccessors.name)

  private def checkDef(tree: Tree)(implicit ctx: Context): Tree = {
    if (tree.symbol.defTree.isEmpty)
      report.error("cannot get tree for " + tree.show, tree.sourcePos)
    tree
  }

  private def checkable(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.exists && !sym.isOneOf(Flags.Package) && !sym.isOneOf(Flags.Param) &&
      (sym.isClass || !sym.isOneOf(Flags.Case, butNot = Flags.Enum)) // pattern-bound symbols

  private def checkRef(tree: Tree)(implicit ctx: Context): Tree =
    if (!checkable(tree.symbol)) tree
    else {
      val helloPkgSym = requiredPackage("hello").moduleClass
      val libPkgSym = requiredPackage("lib").moduleClass
      val enclosingPkg = tree.symbol.enclosingPackageClass

      if (enclosingPkg == helloPkgSym) {  // source code
        checkDef(tree)
        report.warning("tree: " + tree.symbol.defTree.show)
      }
      else if (enclosingPkg == libPkgSym) { // tasty from library
        checkDef(tree)
        // check that all sub-definitions have trees set properly
        // make sure that are no cycles in the code
        transformAllDeep(tree.symbol.defTree)
        report.warning("tree: " + tree.symbol.defTree.show)
      }
      else {
        report.warning(s"${tree.symbol} is neither in lib nor hello, owner = $enclosingPkg", tree.sourcePos)
      }
      tree
    }

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = checkDef(tree)

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = checkDef(tree)

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = checkDef(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree = checkRef(tree)

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree = checkRef(tree)

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context): Tree = {
    tree.tpe.foreachPart {
      case tp: NamedType => checkRef(TypeTree(tp))
      case _ =>
    }
    tree
  }
}
