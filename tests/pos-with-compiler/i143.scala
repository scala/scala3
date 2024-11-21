package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.Denotations.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Contexts.*

class TC5(val ctx: Context) extends AnyVal {
  def candidates(mbr: SingleDenotation): Boolean = {
    mbr.symbol.denot(using ctx).exists
  }
}
