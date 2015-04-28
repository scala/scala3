package dotty.tools
package dotc

import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.pickling.{TastyBuffer, TastyPickler}
import util.SourceFile
import ast.{tpd, untpd}
import TastyBuffer._
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

  /**
   * Addresses in TASTY file of trees, stored by pickling.
   * Note that trees are checked for reference equality,
   * so one can reliably use this function only dirrectly after `pickler`
   */
  var addrOfTree: tpd.Tree => Option[Addr] = (_ => None)

  /**
   * Addresses in TASTY file of symbols, stored by pickling.
   * Note that trees are checked for reference equality,
   * so one can reliably use this function only dirrectly after `pickler`
   */
  var addrOfSym: Symbol => Option[Addr] = (_ => None)
}
