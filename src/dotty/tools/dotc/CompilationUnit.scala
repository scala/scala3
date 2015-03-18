package dotty.tools
package dotc

import dotty.tools.dotc.core.pickling.{TastyBuffer, TastyPickler}
import util.SourceFile
import ast.{tpd, untpd}
import TastyBuffer._

class CompilationUnit(val source: SourceFile) {

  override def toString = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  def isJava = source.file.name.endsWith(".java")
  
  lazy val pickled: TastyPickler = new TastyPickler()

  var addrOfTree: tpd.Tree => Option[Addr] = (_ => None)
}