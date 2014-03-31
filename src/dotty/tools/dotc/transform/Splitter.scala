package dotty.tools.dotc
package transform

import TreeTransforms._
import ast.Trees._
import core.Contexts._
import core.Types._

/** This transform makes usre every identifier and select node
 *  carries a symbol. To do this, certain qualifiers with a union type
 *  have to be "splitted" with a type test.
 *
 *  For now, only self references are treated.
 */
class Splitter extends TreeTransform {
  import ast.tpd._

  override def name: String = "splitter"

  /** Replace self referencing idents with ThisTypes. */
  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) = tree.tpe match {
    case ThisType(cls) =>
      println(s"owner = ${ctx.owner}, context = ${ctx}")
      This(cls) withPos tree.pos
    case _ => tree
  }
}