package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type

import scala.collection.mutable

class MethodSummaryBuilder(val methodDef: Symbol, argumentStoredToHeap: List[Boolean]) {

  private val methodsCalled: mutable.Map[Type, List[CallInfo]] = mutable.Map.empty
  private val argumentReturned: Byte = -1 // -1 if not known

  private var thisAccessed: Boolean = false
  private var accessedModules: List[Symbol] = Nil

  def setThisAccessed(b: Boolean): Unit = {
    thisAccessed = b
  }

  def addAccessedModules(sym: Symbol): Unit = {
    accessedModules = sym :: accessedModules
  }

  def addMethodsCalledBy(tpe: Type, methods: List[CallInfo]): Unit = {
    methodsCalled(tpe) = methods ::: methodsCalled.getOrElse(tpe, Nil)
  }

  def result(): MethodSummary =
    MethodSummary(methodDef, thisAccessed, methodsCalled.toMap, accessedModules, argumentReturned, argumentStoredToHeap)
}
