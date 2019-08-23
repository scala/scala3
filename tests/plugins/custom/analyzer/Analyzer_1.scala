// Similar code resides in scripted tests, which only runs on nightly:
//
// sbt-dotty/sbt-test/sbt-dotty/analyzer-plugin/plugin
//
// You may want to change the code there too

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
import Types._
import transform.{ReifyQuotes, FirstTransform}

class SetDefTree extends PluginPhase {
  import tpd._

  override val phaseName: String = SetDefTree.name
  override def runsAfter: Set[String] = Set(ReifyQuotes.name)
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
  import tpd._

  val name: String = "initChecker"
  override val description: String = "checks that under -Yretain-trees we may get tree for all symbols"

  val phaseName = name

  override val runsAfter = Set(SetDefTree.name)
  override val runsBefore = Set(FirstTransform.name)

  def init(options: List[String]): List[PluginPhase] = this :: (new SetDefTree) :: Nil

  private def checkDef(tree: Tree)(implicit ctx: Context): Tree = {
    if (tree.symbol.defTree.isEmpty)
      ctx.error("cannot get tree for " + tree.show, tree.sourcePos)
    tree
  }

  private def checkable(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.exists && !sym.isOneOf(Flags.Package) && !sym.isOneOf(Flags.Param) &&
      (sym.isClass || !sym.isOneOf(Flags.Case, butNot = Flags.Enum)) // pattern-bound symbols

  private def checkRef(tree: Tree)(implicit ctx: Context): Tree =
    if (!checkable(tree.symbol)) tree
    else {
      val helloPkgSym = ctx.requiredPackage("hello").moduleClass
      val libPkgSym = ctx.requiredPackage("lib").moduleClass
      val enclosingPkg = tree.symbol.enclosingPackageClass

      if (enclosingPkg == helloPkgSym) {  // source code
        checkDef(tree)
        ctx.warning("tree: " + tree.symbol.defTree.show)
      }
      else if (enclosingPkg == libPkgSym) { // tasty from library
        checkDef(tree)
        // check that all sub-definitions have trees set properly
        // make sure that are no cycles in the code
        transformAllDeep(tree.symbol.defTree)
        ctx.warning("tree: " + tree.symbol.defTree.show)
      }
      else {
        ctx.warning(tree.symbol + " is neither in lib nor hello, owner = " + enclosingPkg, tree.sourcePos)
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