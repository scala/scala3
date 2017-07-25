package dotty.tools
package dotc

import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.tasty.{TastyUnpickler, TastyBuffer, TastyPickler}
import util.SourceFile
import ast.{tpd, untpd}
import dotty.tools.dotc.ast.tpd.{ Tree, TreeTraverser }
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Symbols._

class CompilationUnit(val source: SourceFile) {

  override def toString = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  def isJava = source.file.name.endsWith(".java")

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, Array[Byte]] = Map()
}

object CompilationUnit {

  /** Make a compilation unit for top class `clsd` with the contends of the `unpickled` */
  def mkCompilationUnit(clsd: ClassDenotation, unpickled: Tree)(implicit ctx: Context): CompilationUnit = {
    val unit1 = new CompilationUnit(new SourceFile(clsd.symbol.sourceFile, Seq()))
    unit1.tpdTree = unpickled
    force.traverse(unit1.tpdTree)
    unit1
  }

  /** Force the tree to be loaded */
  private object force extends TreeTraverser {
    def traverse(tree: Tree)(implicit ctx: Context): Unit = traverseChildren(tree)
  }
}
