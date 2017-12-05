package dotty.tools.dotc.interpreter

import dotty.tools.dotc.ast.tpd

class RawType(val tree: tpd.Tree) extends quoted.Type[Any] with RawQuoted {
  override def toString: String = s"RawType(${tree.toString})"
}
