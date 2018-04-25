package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Flags, Types}

import scala.tasty.types

object MethodType {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.MethodType): types.MethodType = new Impl(tpe)

  def unapplyMethodType(arg: Impl)(implicit ctx: Context): Option[types.MethodType.Data] = {
    val meth = arg.meth
    Some((meth.paramNames.map(TermName(_)), meth.paramInfos.map(Type(_)), Type(meth.resType)))
  }

  private[tasty] class Impl(val meth: Types.MethodType) extends types.MethodType {

    override def isImplicit: Boolean = meth.isImplicitMethod
    override def isErased: Boolean = meth.isErasedMethod

    override def toString: String = "MethodType"
  }
}
