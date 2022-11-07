package dotty.tools.dotc
package transform

import scala.language.unsafeNulls

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.config.Printers.{macroAnnot => debug}
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.MacroClassLoader
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.quoted.*
import dotty.tools.dotc.util.SrcPos
import scala.quoted.runtime.impl.{QuotesImpl, SpliceScope}

import scala.quoted.Quotes

class MacroAnnotations(thisPhase: DenotTransformer):
  import tpd.*
  import MacroAnnotations.*

  /** Expands every macro annotation that is on this tree.
   *  Returns a list with transformed definition and any added definitions.
   */
  def expandAnnotations(tree: MemberDef)(using Context): List[DefTree] =
    if !hasMacroAnnotation(tree.symbol) then
      List(tree)
    else if tree.symbol.is(Module) then
      if tree.symbol.isClass then // error only reported on module class
        report.error("macro annotations are not supported on object", tree)
      List(tree)
    else if tree.symbol.isClass then
      report.error("macro annotations are not supported on class", tree)
      List(tree)
    else if tree.symbol.isType then
      report.error("macro annotations are not supported on type", tree)
      List(tree)
    else
      debug.println(i"Expanding macro annotations of:\n$tree")

      val macroInterpreter = new Interpreter(tree.srcPos, MacroClassLoader.fromContext)

      val allTrees = List.newBuilder[DefTree]
      var insertedAfter: List[List[DefTree]] = Nil

      // Apply all macro annotation to `tree` and collect new definitions in order
      val transformedTree: DefTree = tree.symbol.annotations.foldLeft(tree) { (tree, annot) =>
        if isMacroAnnotation(annot) then
          debug.println(i"Expanding macro annotation: ${annot}")

          // Interpret call to `new myAnnot(..).transform(using <Quotes>)(<tree>)`
          val transformedTrees = callMacro(macroInterpreter, tree, annot)
          transformedTrees.span(_.symbol != tree.symbol) match
            case (prefixed, newTree :: suffixed) =>
              allTrees ++= prefixed
              insertedAfter = suffixed :: insertedAfter
              prefixed.foreach(checkAndEnter(_, tree.symbol, annot))
              suffixed.foreach(checkAndEnter(_, tree.symbol, annot))
              newTree
            case (Nil, Nil) =>
              report.error(i"Unexpected `Nil` returned by `(${annot.tree}).transform(..)` during macro expansion", annot.tree.srcPos)
              tree
            case (_, Nil) =>
              report.error(i"Transformed tree for ${tree} was not return by `(${annot.tree}).transform(..)` during macro expansion", annot.tree.srcPos)
              tree
        else
          tree
      }

      allTrees += transformedTree
      insertedAfter.foreach(allTrees.++=)

      val result = allTrees.result()
      debug.println(result.map(_.show).mkString("expanded to:\n", "\n", ""))
      result

  /** Interpret the code `new annot(..).transform(using <Quotes(ctx)>)(<tree>)` */
  private def callMacro(interpreter: Interpreter, tree: MemberDef, annot: Annotation)(using Context): List[MemberDef] =
    // TODO: Remove when scala.annaotaion.MacroAnnotation is no longer experimental
    import scala.reflect.Selectable.reflectiveSelectable
    type MacroAnnotation = {
      def transform(using Quotes)(tree: Object/*Erased type of quotes.refelct.Definition*/): List[MemberDef /*quotes.refelct.Definition known to be MemberDef in QuotesImpl*/]
    }

    // Interpret macro annotation instantiation `new myAnnot(..)`
    val annotInstance = interpreter.interpret[MacroAnnotation](annot.tree).get
    // TODO: Remove when scala.annaotaion.MacroAnnotation is no longer experimental
    assert(annotInstance.getClass.getClassLoader.loadClass("scala.annotation.MacroAnnotation").isInstance(annotInstance))

    val quotes = QuotesImpl()(using SpliceScope.contextWithNewSpliceScope(tree.symbol.sourcePos)(using MacroExpansion.context(tree)).withOwner(tree.symbol))
    annotInstance.transform(using quotes)(tree.asInstanceOf[quotes.reflect.Definition])

  /** Check that this tree can be added by the macro annotation and enter it if needed */
  private def checkAndEnter(newTree: Tree, annotated: Symbol, annot: Annotation)(using Context) =
    val sym = newTree.symbol
    if sym.isClass then
      report.error("Generating classes is not supported", annot.tree)
    else if sym.isType then
      report.error("Generating type is not supported", annot.tree)
    else if sym.owner != annotated.owner then
      report.error(i"macro annotation $annot added $sym with an inconsistent owner. Expected it to be owned by ${annotated.owner} but was owned by ${sym.owner}.", annot.tree)
    else
      sym.enteredAfter(thisPhase)

object MacroAnnotations:

  /** Is this an annotation that implements `scala.annation.MacroAnnotation` */
  def isMacroAnnotation(annot: Annotation)(using Context): Boolean =
    annot.tree.symbol.maybeOwner.derivesFrom(defn.MacroAnnotationClass)

  /** Is this symbol annotated with an annotation that implements `scala.annation.MacroAnnotation` */
  def hasMacroAnnotation(sym: Symbol)(using Context): Boolean =
    sym.getAnnotation(defn.MacroAnnotationClass).isDefined
