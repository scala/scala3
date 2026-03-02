package dotty.tools.dotc
package transform

import scala.language.unsafeNulls

import ast.tpd
import ast.Trees.*
import config.Printers.macroAnnot as debug
import core.Annotations.*
import core.Contexts.*
import core.Decorators.*
import core.DenotTransformers.DenotTransformer
import core.Flags.*
import core.MacroClassLoader
import core.Symbols.*
import core.Types.*
import quoted.*
import util.SrcPos
import scala.quoted.runtime.impl.{QuotesImpl, SpliceScope}

import scala.quoted.Quotes
import scala.util.control.NonFatal

import java.lang.reflect.InvocationTargetException

object MacroAnnotations:

  import tpd.*

  extension (annot: Annotation)
    /** Is this an annotation that implements `scala.annation.MacroAnnotation` */
    def isMacroAnnotation(using Context): Boolean =
      annot.tree.symbol.maybeOwner.derivesFrom(defn.MacroAnnotationClass)
  end extension

  extension (sym: Symbol)
    /** Is this symbol annotated with an annotation that implements `scala.annation.MacroAnnotation` */
    def hasMacroAnnotation(using Context): Boolean =
      sym.getAnnotation(defn.MacroAnnotationClass).isDefined
  end extension

  /** Expands every macro annotation that is on this tree.
   *  Returns a list with transformed definition and any added definitions.
   */
  def expandAnnotations(tree: MemberDef, companion: Option[MemberDef])(using Context): (List[MemberDef], Option[MemberDef]) =
    if !tree.symbol.hasMacroAnnotation then
      (List(tree), companion)
    else if tree.symbol.is(ModuleVal) then
      // only module classes are transformed
      (List(tree), companion)
    else if tree.symbol.isType && !tree.symbol.isClass then
      report.error("macro annotations are not supported on type", tree)
      (List(tree), companion)
    else
      debug.println(i"Expanding macro annotations of:\n$tree")
      val macroInterpreter = new Interpreter(tree.srcPos, MacroClassLoader.fromContext)

      val prefixedTrees = List.newBuilder[MemberDef]

      // Apply all macro annotation to `tree` and collect new definitions in order
      val unprocessed = (tree, companion, List.empty[MemberDef])
      val (transformedTree, transformedCompanion, suffixed) =
        tree.symbol.annotations.foldLeft(unprocessed): (lastResult, annot) =>
          if annot.isMacroAnnotation then
            val (tree, companion, suffixed) = lastResult
            debug.println(i"Expanding macro annotation: ${annot}")
            // Interpret call to `new myAnnot(..).transform(using <Quotes>)(<tree>, <companion>)`
            val (transformedTrees, transformedCompanion) = callMacro(macroInterpreter, tree, companion, annot)
            // Establish the trees order and check the integrity of the trees
            transformedTrees.span(_.symbol != tree.symbol) match
              case (newPrefixed, newTree :: newSuffixed) =>
                // Check the integrity of the generated trees
                for prefixedTree <- newPrefixed do checkMacroDef(prefixedTree, tree, annot)
                for suffixedTree <- newSuffixed do checkMacroDef(suffixedTree, tree, annot)
                for tcompanion <- transformedCompanion do TreeChecker.checkMacroGeneratedTree(companion.get, tcompanion)
                TreeChecker.checkMacroGeneratedTree(tree, newTree)
                prefixedTrees ++= newPrefixed
                (newTree, transformedCompanion, newSuffixed ::: suffixed)
              case (_, Nil) =>
                report.error(i"Transformed tree for ${tree.symbol} was not return by `(${annot.tree}).transform(..)` during macro expansion", annot.tree.srcPos)
                lastResult
          else
            lastResult
      end val

      // Complete the list of transformed/generated definitions
      val result = prefixedTrees.result() ::: transformedTree :: suffixed
      debug.println(result.map(_.show).mkString("expanded to:\n", "\n", ""))
      (result, transformedCompanion)
  end expandAnnotations

  /** Interpret the code `new annot(..).transform(using <Quotes(ctx)>)(<tree>, <companion>)` */
  private def callMacro(interpreter: Interpreter, tree: MemberDef, companion: Option[MemberDef], annot: Annotation)
                       (using Context): (List[MemberDef], Option[MemberDef]) =
    // TODO: Remove when scala.annotation.MacroAnnotation is no longer experimental
    import scala.reflect.Selectable.reflectiveSelectable
    type MacroAnnotation = {
      def transform(using Quotes)(
        tree: Object/*Erased type of quotes.reflect.Definition*/,
        companion: Option[Object/*Erased type of quotes.reflect.Definition*/]
      ): List[MemberDef /*quotes.refelct.Definition known to be MemberDef in QuotesImpl*/]
    }

    // Interpret macro annotation instantiation `new myAnnot(..)`
    // TODO: Make this error handling stronger (no error handling at the moment)
    val annotInstance = interpreter.interpret[MacroAnnotation](annot.tree).get

    // TODO: Remove when scala.annaotaion.MacroAnnotation is no longer experimental
    assert(annotInstance.getClass.getClassLoader.loadClass("scala.annotation.MacroAnnotation").isInstance(annotInstance))

    val quotes = QuotesImpl()(using SpliceScope.contextWithNewSpliceScope(tree.symbol.sourcePos)(using MacroExpansion.context(tree)).withOwner(tree.symbol.owner))
    try
      val result = annotInstance.transform(using quotes)(tree, companion)
      // Process the result based on if the companion was present or not
      // The idea is that we try to find a transformation of the companion if we do provide one
      companion.map(_.symbol) match
        case None => (result, companion)
        case Some(companionSym) => result.partition(_.symbol == companionSym) match
          case (Nil, result) => (result, companion) // companion didn't change
          case (newCompanion :: Nil, result) => (result, Some(newCompanion))
          case (_, result) =>
            report.error(i"Transformed companion for ${tree.symbol} was returned more than once by `(${annot.tree}).transform(..)` during macro expansion", annot.tree)
            (result, companion)

    catch
      // TODO: Replace this case when scala.annotation.MacroAnnotation is no longer experimental and reflectiveSelectable is not used
      //       Replace this case with the nested cases.
      case ex0: InvocationTargetException =>
        ex0.getCause match
          case ex: CompilationUnit.SuspendException =>
            throw ex
          case ex: scala.quoted.runtime.StopMacroExpansion =>
            if !ctx.reporter.hasErrors then
              report.error("Macro expansion was aborted by the macro without any errors reported. Macros should issue errors to end-users when aborting a macro expansion with StopMacroExpansion.", annot.tree)
            (List(tree), companion)
          case Interpreter.MissingClassValidInCurrentRun(sym, origin) =>
            Interpreter.suspendOnMissing(sym, origin, annot.tree)
          case NonFatal(ex) =>
            val stack0 = ex.getStackTrace.takeWhile(_.getClassName != this.getClass().getName())
            val stack = stack0.take(1 + stack0.lastIndexWhere(_.getMethodName == "transform"))
            val msg =
              em"""Failed to evaluate macro annotation '$annot'.
                  |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
                  |    ${stack.mkString("\n    ")}
                  |"""
            report.error(msg, annot.tree)
            (List(tree), companion)
          case _ =>
            throw ex0
  end callMacro

  /** Check that this tree can be added by the macro annotation */
  private def checkMacroDef(newTree: DefTree, annotatedTree: Tree, annot: Annotation)(using Context) =
    TreeChecker.checkMacroGeneratedTree(annotatedTree, newTree)
    val sym = newTree.symbol
    val annotated = annotatedTree.symbol
    if sym.isType && !sym.isClass then
      report.error(i"macro annotation cannot return a `type`. $annot tried to add $sym", annot.tree)
    else if sym.owner != annotated.owner && !(annotated.owner.isPackageObject && (sym.isClass || sym.is(Module)) && sym.owner == annotated.owner.owner) then
      report.error(i"macro annotation $annot added $sym with an inconsistent owner. Expected it to be owned by ${annotated.owner} but was owned by ${sym.owner}.", annot.tree)
    else if annotated.isClass && annotated.owner.is(Package) /*&& !sym.isClass*/ then
      report.error(i"macro annotation can not add top-level ${sym.showKind}. $annot tried to add $sym.", annot.tree)
  end checkMacroDef

  /** Enter the symbols generated by MacroAnnotations */
  def enterMissingSymbols(tree: MemberDef, phase: DenotTransformer)(using Context) = new TreeTraverser {
    def traverse(tree: tpd.Tree)(using Context): Unit = tree match
      case tdef @ TypeDef(_, template: Template) =>
        val isSymbolInDecls = atNextPhase(tdef.symbol.asClass.info.decls.toList.toSet)
        for tree <- template.body if tree.isDef do
          if tree.symbol.owner != tdef.symbol then
            report.error(em"Macro added a definition with the wrong owner - ${tree.symbol.owner} - ${tdef.symbol} in ${tree.source}", tree.srcPos)
          else if !isSymbolInDecls(tree.symbol) then
            tree.symbol.enteredAfter(phase)
        traverseChildren(tree)
      case _ => traverseChildren(tree)
  }.traverse(tree)

end MacroAnnotations
