package dotty.tools.dotc
package transform

import core._
import Symbols._, Contexts._, Types._, Names._, StdNames._
import ast._
import Trees._
import TypeUtils._

object TreeGen {

  import tpd._

  def wrapArrayMethodName(elemtp: Type)(implicit ctx: Context): TermName = {
    val elemCls = elemtp.classSymbol
    if (elemCls.isPrimitiveValueClass) nme.wrapXArray(elemCls.name)
    else if (elemCls.derivesFrom(defn.ObjectClass) && !elemCls.isNotRuntimeClass) nme.wrapRefArray
    else nme.genericWrapArray
  }

  def wrapArray(tree: Tree, elemtp: Type)(implicit ctx: Context): Tree =
    ref(defn.ScalaPredefModule)
      .select(wrapArrayMethodName(elemtp))
      .appliedToTypes(if (elemtp.isPrimitiveValueType) Nil else elemtp :: Nil)
      .appliedTo(tree)
}
