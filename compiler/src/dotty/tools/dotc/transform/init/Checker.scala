package dotty.tools.dotc
package transform
package init

import dotty.tools.dotc.*
import ast.tpd
import tpd.*

import dotty.tools.dotc.core.*
import Contexts.*
import Types.*
import Symbols.*
import StdNames.*

import dotty.tools.dotc.transform.*
import Phases.*

import scala.collection.mutable

import Semantic.*
import dotty.tools.unsupported

class Checker extends Phase:

  override def phaseName: String = Checker.name

  override def description: String = Checker.description

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Context): Boolean =
    super.isEnabled && (ctx.settings.Whas.safeInit || ctx.settings.YsafeInitGlobal.value)

  def traverse(traverser: InitTreeTraverser)(using Context): Boolean = monitor(phaseName):
    val unit = ctx.compilationUnit
    traverser.traverse(unit.tpdTree)

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val checkCtx = ctx.fresh.setPhase(this)
    val traverser = new InitTreeTraverser()

    val units0 =
      for
        unit <- units
        unitContext = checkCtx.fresh.setCompilationUnit(unit)
        if traverse(traverser)(using unitContext)
      yield
        unitContext.compilationUnit

    cancellable {
      val classes = traverser.getClasses()

      if ctx.settings.Whas.safeInit then
        Semantic.checkClasses(classes)(using checkCtx)

      if ctx.settings.YsafeInitGlobal.value then
        val obj = new Objects
        obj.checkClasses(classes)(using checkCtx)
    }

    units0
  end runOn

  override protected def run(using Context): Unit = unsupported("run")

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
