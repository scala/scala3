package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Flags, Types}

import scala.tasty.types

object MethodType {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.MethodType)(implicit ctx: Context): types.MethodType = new Impl(tpe)

  def unapplyMethodType(arg: Impl): Option[types.MethodType.Data] = {
    implicit val ctx: Context = arg.ctx
    val meth = arg.meth
    Some((meth.paramNames.map(TermName(_)), meth.paramInfos.map(Type(_)), Type(meth.resType)))
  }

  private[tasty] class Impl(val meth: Types.MethodType)(implicit val ctx: Context) extends types.MethodType {

    override def isImplicit: Boolean = meth.isImplicitMethod
    override def isErased: Boolean = meth.isErasedMethod

    override def toString: String = {
      import Toolbox.extractor
      val types.MethodType(paramNames, paramTypes, resType) = this
      s"MethodType($paramNames, $paramTypes, $resType)"
    }
  }
}
