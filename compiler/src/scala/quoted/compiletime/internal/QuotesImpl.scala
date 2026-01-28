package scala.quoted.compiletime.internal

import dotty.tools.dotc.core.Contexts.*
import scala.quoted.compiletime as pub

object QuotesImpl {

  def apply()(using Context): pub.Quotes =
    new QuotesImpl

  // FIX-PRE-MERGE (KR) :
  /*
  def showDecompiledTree(tree: tpd.Tree)(using Context): String =
    import qctx.reflect.Printer.{TreeCode, TreeAnsiCode}
    val qctx: QuotesImpl = new QuotesImpl(using MacroExpansion.context(tree))
    if ctx.settings.color.value == "always" then TreeAnsiCode.show(tree)
    else TreeCode.show(tree)
   */

}

final class QuotesImpl private (using val ctx: Context) extends pub.Quotes {

  override lazy val reflectV2: reflectImpl.Module = new reflectImpl.Module

}
