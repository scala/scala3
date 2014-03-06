package dotty.tools
package dotc
package typer

import core._
import ast._
import Scopes._, Contexts._, Constants._, Types._, Symbols._

trait TypeAssigner extends NoChecking {

  def assignType(tree: untpd.New)(implicit ctx: Context) = {
    checkClassTypeWithStablePrefix(tree.tpt.tpe, tree.tpt.pos, traitReq = false)
    tree.withType(tree.tpt.tpe)
  }

  def assignType(tree: untpd.Literal)(implicit ctx: Context) =
    tree.withType {
      tree.const.tag match {
        case UnitTag => defn.UnitType
        case NullTag => defn.NullType
        case _ => ConstantType(tree.const)
      }
    }
}

object TypeAssigner extends TypeAssigner

