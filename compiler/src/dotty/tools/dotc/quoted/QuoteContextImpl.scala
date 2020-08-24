package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts._

import scala.quoted.QuoteContext

object QuoteContextImpl {

  def apply()(using Context): QuoteContext =
    new QuoteContextImpl(reflect.ReflectionImpl(ctx))

  type ScopeId = Int

  private[dotty] def checkScopeId(id: ScopeId)(using Context): Unit =
    if (id != scopeId)
      throw new scala.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  // TODO Explore more fine grained scope ids.
  //      This id can only differentiate scope extrusion from one compiler instance to another.
  private[dotty] def scopeId(using Context): ScopeId =
    ctx.outersIterator.toList.last.hashCode()
}

class QuoteContextImpl(val tasty: scala.tasty.Reflection) extends QuoteContext
