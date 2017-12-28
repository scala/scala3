package dotty.tools.dotc.interpreter

import dotty.tools.dotc.ast.tpd

/** Expression `quoted.Expr[_]` for which its internal representation is its tree. */
final class RawExpr(val tree: tpd.Tree) extends quoted.Expr[Any] with RawQuoted
