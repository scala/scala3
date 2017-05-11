package dotty.tools
package dotc

import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.tasty.{TastyUnpickler, TastyBuffer, TastyPickler}
import util.SourceFile
import ast.{tpd, untpd}
import dotty.tools.dotc.core.Symbols._

class CompilationUnit(val source: SourceFile) {

  override def toString = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  def isJava = source.file.name.endsWith(".java")

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, Array[Byte]] = Map()
}
