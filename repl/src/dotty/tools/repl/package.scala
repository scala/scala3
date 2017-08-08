package dotty.tools

import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotc.core.Denotations.Denotation

import dotc.reporting.{
  StoreReporter,
  UniqueMessagePositions,
  HideNonSensicalMessages
}

package object repl {
  /** Create empty outer store reporter */
  private[repl] def storeReporter: StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages

  private[repl] implicit class ListOps[A](xs: List[A]) extends AnyVal {
    def intersperse(a: A): List[A] = {
      def recur(xs: List[A]): List[A] = xs match {
        case x :: Nil => List(x)
        case xs => List(xs.head, a) ++ recur(xs.tail)
      }
      recur(xs)
    }
  }

  private[repl] implicit class ShowUser(ds: Denotation | Symbol) extends AnyVal {
    def showUser(implicit ctx: Context): String = {
      val printer = new UserFacingPrinter(ctx)
      val text = ds match {
        case d: Denotation => printer.dclText(d.symbol)
        case s: Symbol => printer.dclText(s)
      }

      text.mkString(ctx.settings.pageWidth.value)
    }
  }
}
