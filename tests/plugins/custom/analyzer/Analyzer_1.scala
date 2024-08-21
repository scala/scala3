// Similar code resides in scripted tests:
//
// sbt-test/analyzer-plugin/plugin
//
// You may want to change the code there too

package analyzer

import scala.language.implicitConversions

import dotty.tools.dotc.*
import core.*
import Contexts.Context
import plugins.*
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators.*
import Symbols.{Symbol, requiredPackage}
import Constants.Constant
import Types.*
import transform.{PickleQuotes, FirstTransform}

class SetDefTree extends PluginPhase {
  import tpd.*

  override val phaseName: String = SetDefTree.name
  override def runsAfter: Set[String] = Set(PickleQuotes.name)
  override def runsBefore: Set[String] = Set(FirstTransform.name)
    // don't allow plugins to change tasty
    // research plugins can still change the phase plan at will

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = tree.setDefTree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = tree.setDefTree

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = tree.setDefTree
}

object SetDefTree {
  val name: String = "SetDefTree"
}

class InitChecker extends PluginPhase with StandardPlugin {
  import tpd.*

  val name: String = "initChecker"
  override val description: String = "checks that under -Yretain-trees we may get tree for all symbols"

  val phaseName = name

  override val runsAfter = Set(SetDefTree.name)
  override val runsBefore = Set(FirstTransform.name)

  override def initialize(options: List[String])(using Context): List[PluginPhase] = this :: (new SetDefTree) :: Nil

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
        report.warning(tree.symbol.toString + " is neither in lib nor hello, owner = " + enclosingPkg, tree.sourcePos)
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