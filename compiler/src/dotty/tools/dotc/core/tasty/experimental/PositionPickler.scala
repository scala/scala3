package dotty.tools.dotc.core.tasty.experimental

import dotty.tools.dotc.ast.untpd

import dotty.tools.tasty.experimental.{TastyPickler, PositionPickler => PositionPicklerImpl}
import dotty.tools.tasty.TastyBuffer.Addr

class PositionPickler(pickler: TastyPickler[DottyTasty.type], addrOfTree: untpd.Tree => Addr)
  extends PositionPicklerImpl(DottyTasty)(pickler, addrOfTree)
