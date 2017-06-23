package dotty.tools.dotc.transform.linker.types

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{Type, UncachedProxyType}

class ErazedType extends UncachedProxyType {
  /** The type to which this proxy forwards operations. */
  def underlying(implicit ctx: Context): Type = ctx.definitions.AnyType
}
