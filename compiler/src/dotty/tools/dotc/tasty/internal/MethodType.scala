package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Flags, Types}

import scala.tasty.types

object MethodType {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.MethodType)(implicit ctx: Context): types.MethodType = Impl(tpe, ctx)

  def unapplyMethodType(tpe: types.MaybeType): Option[types.MethodType.Data] = tpe match {
    case Impl(meth: Types.MethodType, ctx) =>
      implicit val ctx_ = ctx
      // TODO add companion
      Some((meth.paramNames.map(TermName(_)), meth.paramInfos.map(Type(_)), Type(meth.resType)))
    case _ => None
  }

  private case class Impl(meth: Types.MethodType, ctx: Context) extends types.MethodType {

    override def isImplicit: Boolean = meth.isImplicitMethod
    override def isErased: Boolean = meth.isErasedMethod

    override def toString: String = {
      import Toolbox.extractor
      val types.MethodType(paramNames, paramTypes, resType) = this
      s"MethodType($paramNames, $paramTypes, $resType)"
    }
  }
}
