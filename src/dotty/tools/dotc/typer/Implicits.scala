package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import util.Positions._
import Contexts._
import Types._
import Flags._
import Denotations._
import NameOps._
import Symbols._
import Types._
import Decorators._
import Names._
import StdNames._
import Constants._
import Inferencing._
import collection.mutable

trait Implicits { self: Typer =>

  import tpd._

  def viewExists(from: Type, to: Type)(implicit ctx: Context): Boolean = (
       !from.isError
    && !to.isError
    && ctx.implicitsEnabled
    && inferView(EmptyTree, from, to, reportAmbiguous = false) != EmptyTree
    )

  def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean)(implicit ctx: Context): Tree =
    inferImplicit(tree, defn.FunctionType(from :: Nil, to), isView = true, reportAmbiguous)

  def inferImplicit(tree: Tree, pt: Type, isView: Boolean, reportAmbiguous: Boolean)(implicit ctx: Context): Tree = ???

}