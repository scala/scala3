package dotty.tools.dotc
package transform
package init


import dotty.tools.dotc._
import ast.tpd

import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._

import dotty.tools.dotc.transform._
import Phases._


import scala.collection.mutable


class Checker extends Phase {
  import tpd._

  val phaseName = "initChecker"

  // cache of class summary
  private val cache = new Cache

  private val cycleChecker = new CycleChecker(cache)

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Context): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    cycleChecker.clean()
    val newUnits = super.runOn(units)
    cycleChecker.checkCyclic()
    newUnits

  val traverser = new TreeTraverser {
    override def traverse(tree: Tree)(using Context): Unit =
      tree match {
        case tdef: TypeDef if tdef.isClassDef =>
          checkClassDef(tdef)
          cycleChecker.classesInCurrentRun += tdef.symbol
          traverseChildren(tree)
        case _ =>
          traverseChildren(tree)
      }
  }

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    traverser.traverse(unit.tpdTree)
  }

  def checkClassDef(tree: TypeDef)(using Context): tpd.Tree = {
    assert(tree.isClassDef)

    val cls = tree.symbol.asClass
    val instantiable: Boolean =
      cls.is(Flags.Module) ||
      !cls.isOneOf(Flags.AbstractOrTrait) && {
        // see `Checking.checkInstantiable` in typer
        val tp = cls.appliedRef
        val stp = SkolemType(tp)
        val selfType = cls.givenSelfType.asSeenFrom(stp, cls)
        !selfType.exists || stp <:< selfType
      }

    // A concrete class may not be instantiated if the self type is not satisfied
    if (instantiable && cls.enclosingPackageClass != defn.StdLibPatchesPackage.moduleClass) {
      implicit val state: Checking.State = Checking.State(
        visited = Set.empty,
        path = Vector.empty,
        thisClass = cls,
        fieldsInited = mutable.Set.empty,
        parentsInited = mutable.Set.empty,
        safePromoted = mutable.Set.empty,
        dependencies = mutable.Set.empty,
        env = Env(ctx.withOwner(cls), cache)
      )

      Checking.checkClassBody(tree)

      cycleChecker.cacheConstructorDependencies(cls.primaryConstructor, state.dependencies.toList)
    }

    tree
  }
}
