package dotty.tools
package dotc

import util.SourceFile
import ast.{tpd, untpd}

class CompilationUnit(val source: SourceFile) {

  override def toString = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

}