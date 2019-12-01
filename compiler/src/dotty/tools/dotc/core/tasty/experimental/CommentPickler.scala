package dotty.tools.dotc.core.tasty.experimental

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import dotty.tools.tasty.experimental.{CommentPickler => CommentPicklerImpl}
import dotty.tools.tasty.TastyBuffer.Addr

class CommentPickler(pickler: TastyPickler, addrOfTree: tpd.Tree => Addr)(implicit ctx: Context)
  extends CommentPicklerImpl(pickler, addrOfTree)
