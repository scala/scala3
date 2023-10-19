package dotty.tools.dotc
package transform
package init

import dotty.tools.dotc._
import ast.tpd
import tpd._

import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._
import StdNames._

import dotty.tools.dotc.transform._
import Phases._

import scala.collection.mutable

import Semantic._
import dotty.tools.unsupported

class Checker extends Phase:

  override def phaseName: String = Checker.name

  override def description: String = Checker.description

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Context): Boolean =
    super.isEnabled && (ctx.settings.YcheckInit.value || ctx.settings.YcheckInitGlobal.value)

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val checkCtx = ctx.fresh.setPhase(this.start)
    val traverser = new InitTreeTraverser()
    for unit <- units do
      checkCtx.run.beginUnit()
      try traverser.traverse(unit.tpdTree)
      finally ctx.run.advanceUnit()
    val classes = traverser.getClasses()

    if ctx.settings.YcheckInit.value then
      Semantic.checkClasses(classes)(using checkCtx)

    if ctx.settings.YcheckInitGlobal.value then
      Objects.checkClasses(classes)(using checkCtx)

    units

  def run(using Context): Unit = unsupported("run")

  class InitTreeTraverser extends TreeTraverser:
    private val classes: mutable.ArrayBuffer[ClassSymbol] = new mutable.ArrayBuffer

    def getClasses(): List[ClassSymbol] = classes.toList

    override def traverse(tree: Tree)(using Context): Unit =
      traverseChildren(tree)
      tree match {
        case mdef: MemberDef =>
          // self-type annotation ValDef has no symbol
          if mdef.name != nme.WILDCARD then
            mdef.symbol.defTree = tree

          mdef match
          case tdef: TypeDef if tdef.isClassDef =>
            val cls = tdef.symbol.asClass
            classes.append(cls)
          case _ =>

        case _ =>
      }
  end InitTreeTraverser

object Checker:
  val name: String = "initChecker"
  val description: String = "check initialization of objects"
