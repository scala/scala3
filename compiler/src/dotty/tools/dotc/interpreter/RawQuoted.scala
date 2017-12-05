package dotty.tools.dotc.interpreter

import dotty.tools.dotc.ast.tpd

trait RawQuoted extends quoted.Quoted {
  def tree: tpd.Tree
}
