package dotty.tools.dotc
package typer

import core.Contexts._
import core.Types._
import core.Symbols._
import typer.ProtoTypes._
import ast.{tpd, untpd}
import ast.Trees._

/** A version of Typer that keeps all symbols defined and referenced in a
 *  previously typed tree.
 *
 *  All definition nodes keep their symbols. All leaf nodes for idents, selects,
 *  and TypeTrees keep their types. Indexing is a no-op.
 *
 *  Otherwise, everything is as in Typer.
 */
class ReTyper extends Typer {
  import tpd._

  protected def promote(tree: untpd.Tree)(implicit ctx: Context): tree.ThisTree[Type] = {
    assert(tree.hasType)
    tree.withType(tree.typeOpt)
  }

  override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
    assert(tree.hasType)
    val qual1 = typed(tree.qualifier, AnySelectionProto)
    untpd.cpy.Select(tree, qual1, tree.name).withType(tree.typeOpt)
  }

  override def typedSelectFromTypeTree(tree: untpd.SelectFromTypeTree, pt: Type)(implicit ctx: Context): SelectFromTypeTree = {
    assert(tree.hasType)
    val qual1 = typed(tree.qualifier, AnySelectionProto)
    untpd.cpy.SelectFromTypeTree(tree, qual1, tree.name).withType(tree.typeOpt)
  }

  override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): TypeTree =
    promote(tree)

  override def typedBind(tree: untpd.Bind, pt: Type)(implicit ctx: Context): Bind = {
    assert(tree.hasType)
    val body1 = typed(tree.body, pt)
    untpd.cpy.Bind(tree, tree.name, body1).withType(tree.typeOpt)
  }

  override def localDummy(cls: ClassSymbol, impl: untpd.Template)(implicit ctx: Context) = impl.symbol

  override def retrieveSym(tree: untpd.Tree)(implicit ctx: Context): Symbol = tree.symbol

  override def localTyper(sym: Symbol) = this

  override def index(trees: List[untpd.Tree])(implicit ctx: Context) = ctx
}