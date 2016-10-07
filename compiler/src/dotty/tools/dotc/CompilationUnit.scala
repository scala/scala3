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

  /**
   * Picklers used to create TASTY sections, indexed by toplevel class to which they belong.
   * Sections: Header, ASTs and Positions are populated by `pickler` phase.
   * Subsequent phases can add new sections.
   */
  var picklers: Map[ClassSymbol, TastyPickler] = Map()

  var unpicklers: Map[ClassSymbol, TastyUnpickler] = Map()
}
