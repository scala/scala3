package dotty.tools.replfilter

import dotty.tools.dotc.ast.untpd
import dotty.tools.repl.ReplFilter


class DemoFilter extends ReplFilter("FORBIDDEN\nThis operation is not permitted, skipping.."):
  def passImpl(stats: List[untpd.Tree]): Option[List[untpd.Tree]] =
    None
