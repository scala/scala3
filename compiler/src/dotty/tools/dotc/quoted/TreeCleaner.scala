package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

/** Clean up quote artifacts from the tree to make it simpler to read.
 *  - Flattens block and remove blocks with not statements
 *  - Inline type aliases in the tree
 */
class TreeCleaner extends tpd.TreeMap {
  import tpd._

  /** List of symbols and their types for type aliases `type T = U` */
  private[this] var aliasesSyms: List[Symbol] = Nil
  private[this] var aliasesTypes: List[Type] = Nil
  private[this] val aliases = newMutableSymbolMap[Tree]

  override def transform(tree: Tree)(implicit ctx: Context): Tree = {
    val tree0 = tree match {
      case TypeDef(_, TypeBoundsTree(lo, hi)) if lo == hi =>
        aliasesSyms = tree.symbol :: aliasesSyms
        aliasesTypes = lo.tpe :: aliasesTypes
        aliases(tree.symbol) = ref(lo.tpe.typeSymbol)
        Literal(Constant(()))
      case _ => tree
    }

    super.transform(tree0) match {
      case tree1: TypeTree => TypeTree(tree1.tpe.subst(aliasesSyms, aliasesTypes))
      case tree1: Ident => aliases.get(tree1.symbol).getOrElse(tree1)
      case tree1 => tree1
    }
  }
}
