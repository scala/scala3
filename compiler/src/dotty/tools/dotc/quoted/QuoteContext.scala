package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.tastyreflect.ReflectionImpl

object QuoteContext {

  def apply() given Context: scala.quoted.QuoteContext =
    new scala.quoted.QuoteContext(ReflectionImpl(the[Context]))

}
