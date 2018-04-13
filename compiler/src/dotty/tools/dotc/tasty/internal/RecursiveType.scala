package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object RecursiveType {

  // TODO make sure all extractors are tested

  def apply(bounds: Types.RecType)(implicit ctx: Context): types.RecursiveType = Impl(bounds, ctx)

  def unapplyRecursiveType(tpe: types.MaybeType): Option[types.RecursiveType.Data] = tpe match {
    case Impl(tpe: Types.RecType, ctx) =>
      implicit val ctx_ = ctx
      Some(Type(tpe.underlying))
    case _ => None
  }

  private case class Impl(bounds: Types.RecType, ctx: Context) extends types.RecursiveType {
    override def toString: String = {
      import Toolbox.extractor
      val types.RecursiveType(binder) = this
      s"RecursiveType($binder)"
    }
  }
}
