package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object RecursiveType {

  // TODO make sure all extractors are tested

  def apply(binder: Types.RecType): types.RecursiveType = new Impl(binder)

  def unapplyRecursiveType(arg: Impl)(implicit ctx: Context): Option[types.RecursiveType.Data] =
    Some(Type(arg.binder.underlying))

  private[tasty] class Impl(val binder: Types.RecType) extends types.RecursiveType {
    override def toString: String = "RecursiveType"
  }
}
