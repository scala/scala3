package dotty.tools.dotc
package util

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.transform.SymUtils._

object Experimental:
  import tpd._

  def checkExperimental(tree: Tree)(using Context): Unit =
    if tree.symbol.isExperimental
      && !tree.symbol.isConstructor // already reported on the class
      && !ctx.owner.isExperimental // already reported on the @experimental of the owner
      && !tree.symbol.is(ModuleClass) // already reported on the module
      && (tree.span.exists || tree.symbol != defn.ExperimentalAnnot) // already reported on inferred annotations
    then
      Feature.checkExperimentalDef(tree.symbol, tree)

  def checkExperimentalTypes(tree: Tree)(using Context): Unit =
    val checker = new TypeTraverser:
      def traverse(tp: Type): Unit =
        if tp.typeSymbol.isExperimental then
          Feature.checkExperimentalDef(tp.typeSymbol, tree)
        else
          traverseChildren(tp)
    if !tree.span.isSynthetic then // avoid double errors
      checker.traverse(tree.tpe)

  def annotateExperimental(sym: Symbol)(using Context): Unit =
    if sym.isClass && !sym.hasAnnotation(defn.ExperimentalAnnot) then
      // Add @experimental annotation to all classes nested in an experimental definition
      if !sym.owner.is(Package) && sym.isNestedInExperimental then
        sym.addAnnotation(defn.ExperimentalAnnot)

      // Add @experimental annotation to enum class definitions
      val compSym = sym.companionClass
      if compSym.is(Enum) && compSym.hasAnnotation(defn.ExperimentalAnnot) then
        sym.addAnnotation(defn.ExperimentalAnnot)
        sym.companionModule.addAnnotation(defn.ExperimentalAnnot)