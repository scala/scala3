package dotty.tools.dotc.core.tasty.experimental

import dotty.tools.dotc.ast.untpd

import dotty.tools.tasty.experimental.{PositionPickler => PositionPicklerImpl}
import dotty.tools.tasty.TastyBuffer.Addr

class PositionPickler(pickler: TastyPickler, addrOfTree: untpd.Tree => Addr)
  extends PositionPicklerImpl(pickler, addrOfTree)
