package dotty.tools
package dotc
package printing

import core.*
import Texts.*
import Flags.*
import NameOps.*
import StdNames.*
import Contexts.*
import Symbols.*
import ast.{Trees, untpd}
import Trees.*

object OutlinePrinter:
  def apply(_ctx: Context): Printer = new OutlinePrinter(_ctx)

/** A printer that elides known standard tree forms from the rhs of def and val.
  * Typically used for printing Java trees which elide the rhs.
  */
class OutlinePrinter private (_ctx: Context) extends RefinedPrinter(_ctx) {

  /* Typical patterns seen in output of typer for Java code, plus the output of unpickling an ELIDED tree */
  def isElidableExpr[T <: Untyped](tree: Tree[T]): Boolean = tree match {
    case tree if tree.isEmpty => false
    case tree: Ident[T] if tree.name == nme.WILDCARD => true // `ELIDED exprType`
    case tree: Literal[T] => true // e.g. `()`
    case tree: Select[T] if tree.symbol == defn.Predef_undefined => true // e.g. `Predef.???`
    case Apply(Select(tree: New[T], nme.CONSTRUCTOR), Nil)
    if tree.tpt.typeOpt.typeSymbol.is(Module) => true // e.g. `new foo.Foo$()` (rhs of a module val)
    case _ =>
      sys.error(s"Unexpected tree in OutlinePrinter: ${tree.show}, $tree")
      false
  }

  override protected def rhsValDef[T <: Untyped](tree: ValDef[T]): Text =
    if isElidableExpr(tree.rhs) then " = " ~ "elided" ~ "[" ~ toText(tree.tpt) ~ "]"
    else super.rhsValDef(tree)

  override protected def rhsDefDef[T <: Untyped](tree: DefDef[T]): Text =
    if isElidableExpr(tree.rhs) then " = " ~ "elided" ~ "["  ~ toText(tree.tpt) ~ "]"
    else super.rhsDefDef(tree)
}
