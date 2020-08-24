package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.quoted.reflect._

import scala.quoted.QuoteContext
import scala.quoted.show.SyntaxHighlight

object QuoteContextImpl {

  type ScopeId = Int

  def apply()(using Context): QuoteContext =
    new QuoteContextImpl(ctx)

  def showTree(tree: tpd.Tree)(using Context): String = {
    val qctx = QuoteContextImpl()(using MacroExpansion.context(tree))
    val syntaxHighlight =
      if (ctx.settings.color.value == "always") SyntaxHighlight.ANSI
      else SyntaxHighlight.plain
    show(using qctx)(tree.asInstanceOf[qctx.tasty.Tree], syntaxHighlight)(using ctx.asInstanceOf[qctx.tasty.Context])
  }

  private def show(using qctx: QuoteContext)(tree: qctx.tasty.Tree, syntaxHighlight: SyntaxHighlight)(using qctx.tasty.Context) =
    tree.showWith(syntaxHighlight)

  private[dotty] def checkScopeId(id: ScopeId)(using Context): Unit =
    if (id != scopeId)
      throw new scala.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  // TODO Explore more fine grained scope ids.
  //      This id can only differentiate scope extrusion from one compiler instance to another.
  private[dotty] def scopeId(using Context): ScopeId =
    ctx.outersIterator.toList.last.hashCode()

}

class QuoteContextImpl private (ctx: Context) extends QuoteContext {
  // NOTE: The tasty class should only mixin the compiler interface and the reflection interface.
  //       We should not implement methods here, all should be implemented by `ReflectionCompilerInterface`
  val tasty = new ReflectionCompilerInterface(ctx) with scala.tasty.Reflection
}
