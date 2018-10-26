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


import dotty.tools.dotc.reporting.diagnostic.MessageContainer
/*
trait WarningTransformer {
  val transformer: MessageContainer => Option[MessageContainer]
  position: Position
}
*/

class CompilationUnit(val source: SourceFile) {

  override def toString: String = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  //var warningTransformers: List[WarningTransformer] = Nil

  def isJava = source.file.name.endsWith(".java")

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, Array[Byte]] = Map()

  /** Will be reset to `true` if `tpdTree` contains a call to an inline method. The information
   *  is used in phase InlineCalls in order to avoid traversing an inline-less tree.
   */
  var containsInlineCalls: Boolean = false

  /** Will be reset to `true` if `untpdTree` contains `Quote` trees. The information
   *  is used in phase ReifyQuotes in order to avoid traversing a quote-less tree.
   */
  var containsQuotesOrSplices: Boolean = false

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
      unit1.containsInlineCalls = force.containsInline
      unit1.containsQuotesOrSplices = force.containsQuotes
    }
    unit1
  }

  /** Force the tree to be loaded */
  private class Force extends TreeTraverser {
    var containsInline = false
    var containsQuotes = false
    def traverse(tree: Tree)(implicit ctx: Context): Unit = {
      if (tree.symbol.isQuote)
        containsQuotes = true
      if (tpd.isInlineCall(tree))
        containsInline = true
      traverseChildren(tree)
    }
  }
}
