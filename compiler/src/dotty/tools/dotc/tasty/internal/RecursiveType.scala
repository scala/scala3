package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object RecursiveType {

  // TODO make sure all extractors are tested

  def apply(binder: Types.RecType)(implicit ctx: Context): types.RecursiveType = new Impl(binder)

  def unapplyRecursiveType(arg: Impl): Option[types.RecursiveType.Data] = {
    implicit val ctx: Context = arg.ctx
    Some(Type(arg.binder.underlying))
  }

  private[tasty] class Impl(val binder: Types.RecType)(implicit val ctx: Context) extends types.RecursiveType {
    override def toString: String = {
      import Toolbox.extractor
      val types.RecursiveType(binder) = this
      s"RecursiveType($binder)"
    }
  }
}
