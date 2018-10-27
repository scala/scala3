package dotty.tools
package dotc

import util.SourceFile
import ast.{tpd, untpd}
import dotty.tools.dotc.ast.Trees
import tpd.{Tree, TreeTraverser}
import typer.PrepareInlineable.InlineAccessors
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.typer.Inliner

class CompilationUnit(val source: SourceFile) {

  override def toString: String = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  def isJava: Boolean = source.file.name.endsWith(".java")

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, Array[Byte]] = Map()

  /** Will be reset to `true` if contains `Quote`, `Splice` or calls to inline methods.
   * The information is used in phase ReifyQuotes in order to avoid traversing a quote-less tree.
   */
  var containsQuotesSplicesOrInline: Boolean = false

  /** A structure containing a temporary map for generating inline accessors */
  val inlineAccessors: InlineAccessors = new InlineAccessors
}

object CompilationUnit {

  /** Make a compilation unit for top class `clsd` with the contends of the `unpickled` */
  def mkCompilationUnit(clsd: ClassDenotation, unpickled: Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit =
    mkCompilationUnit(SourceFile(clsd.symbol.associatedFile, Array.empty), unpickled, forceTrees)

  /** Make a compilation unit, given picked bytes and unpickled tree */
  def mkCompilationUnit(source: SourceFile, unpickled: Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit = {
    assert(!unpickled.isEmpty, unpickled)
    val unit1 = new CompilationUnit(source)
    unit1.tpdTree = unpickled
    if (forceTrees) {
      val force = new Force
      force.traverse(unit1.tpdTree)
      unit1.containsQuotesSplicesOrInline = force.containsQuotesOrInline
    }
    unit1
  }

  /** Force the tree to be loaded */
  private class Force extends TreeTraverser {
    var containsQuotesOrInline = false
    def traverse(tree: Tree)(implicit ctx: Context): Unit = {
      // Note that top-level splices are still inside the inline methods
      if (tree.symbol.isQuote || tpd.isInlineCall(tree))
        containsQuotesOrInline = true
      traverseChildren(tree)
    }
  }
}
