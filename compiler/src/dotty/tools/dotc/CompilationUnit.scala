package dotty.tools
package dotc

import util.SourceFile
import ast.{tpd, untpd}
import tpd.{Tree, TreeTraverser}
import typer.PrepareInlineable.InlineAccessors
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.SymUtils._
import util.{NoSource, SourceFile}
import core.Decorators._

class CompilationUnit protected (val source: SourceFile) {

  override def toString: String = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  def isJava: Boolean = source.file.name.endsWith(".java")

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, Array[Byte]] = Map()

  /** Will be set to `true` if contains `Quote`.
   *  The information is used in phase `Staging` in order to avoid traversing trees that need no transformations.
   */
  var needsStaging: Boolean = false

  /** A structure containing a temporary map for generating inline accessors */
  val inlineAccessors: InlineAccessors = new InlineAccessors

  var suspended: Boolean = false

  def suspend()(given ctx: Context): Nothing =
    if !suspended then
      println(i"suspended: $this")
      suspended = true
      ctx.run.suspendedUnits += this
    throw CompilationUnit.SuspendException()
}

object CompilationUnit {

  class SuspendException extends Exception

  /** Make a compilation unit for top class `clsd` with the contents of the `unpickled` tree */
  def apply(clsd: ClassDenotation, unpickled: Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit =
    apply(new SourceFile(clsd.symbol.associatedFile, Array.empty[Char]), unpickled, forceTrees)

  /** Make a compilation unit, given picked bytes and unpickled tree */
  def apply(source: SourceFile, unpickled: Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit = {
    assert(!unpickled.isEmpty, unpickled)
    val unit1 = new CompilationUnit(source)
    unit1.tpdTree = unpickled
    if (forceTrees) {
      val force = new Force
      force.traverse(unit1.tpdTree)
      unit1.needsStaging = force.needsStaging
    }
    unit1
  }

  def apply(source: SourceFile)(implicit ctx: Context): CompilationUnit = {
    val src =
      if (source.file.isDirectory) {
        ctx.error(s"expected file, received directory '${source.file.path}'")
        NoSource
      }
      else if (!source.file.exists) {
        ctx.error(s"not found: ${source.file.path}")
        NoSource
      }
      else source
    new CompilationUnit(source)
  }

  /** Force the tree to be loaded */
  private class Force extends TreeTraverser {
    var needsStaging = false
    def traverse(tree: Tree)(implicit ctx: Context): Unit = {
      if (tree.symbol.isQuote)
        needsStaging = true
      traverseChildren(tree)
    }
  }
}
