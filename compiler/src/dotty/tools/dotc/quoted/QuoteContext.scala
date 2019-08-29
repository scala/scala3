package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.tastyreflect.ReflectionImpl

object QuoteContext {

  def apply() given Context: scala.quoted.QuoteContext =
    new scala.quoted.QuoteContext(ReflectionImpl(the[Context]))

  type ScopeId = Int

  private[dotty] def checkScopeId(id: ScopeId) given Context: Unit =
    if (id != scopeId)
      throw new scala.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  // TODO Explore more fine grained scope ids.
  //      This id can only differentiate scope extrusion from one compiler instance to another.
  private[dotty] def scopeId given Context: ScopeId =
    the[Context].outersIterator.toList.last.hashCode()
}
