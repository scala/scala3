package scala.quoted.compiletime.internal

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts.*
import scala.quoted.compiletime as pub

type Symbol = SymbolImpl
final case class SymbolImpl(symbol: dotc.core.Symbols.Symbol)(using val ctx: Context) extends pub.Symbol {
  // TODO implementations
}
object SymbolImpl {
  final class Module(using val ctx: Context) extends pub.Symbol.Module {
    // TODO implementations
  }
}
