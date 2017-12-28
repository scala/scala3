package dotty.tools.dotc.interpreter

import dotty.tools.dotc.ast.tpd

/** Type `quoted.Type[_]` for which its internal representation is its type tree. */
final class RawType(val tree: tpd.Tree) extends quoted.Type[Any] with RawQuoted
