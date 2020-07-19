package dottyBench.tools.dotc.quoted

import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.tastyreflect.ReflectionImpl

object QuoteContext {

  def apply()(using Ctx, CState): scala.quoted.QuoteContext =
    new QuoteContext(ReflectionImpl(combinedContext))

  type ScopeId = Int

  private[dottyBench] def checkScopeId(id: ScopeId)(using Ctx, CState): Unit =
    if (id != scopeId)
      throw new scala.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  // TODO Explore more fine grained scope ids.
  //      This id can only differentiate scope extrusion from one compiler instance to another.
  private[dottyBench] def scopeId(using Ctx, CState): ScopeId =
    ctx.outersIterator.toList.last.hashCode()
}

class QuoteContext(val tasty: scala.tasty.Reflection) extends scala.quoted.QuoteContext
