package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.Denotations._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Contexts._

class TC5(val ctx: Context) extends AnyVal {
  def candidates(mbr: SingleDenotation): Boolean = {
    mbr.symbol.denot(ctx).exists
  }
}
