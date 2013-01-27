package dotty.tools.dotc
package core

import Types._, Symbols._, Contexts._, Scopes._

object Printers {

  abstract class Printer {
    def show(tp: Type)(implicit ctx: Context): String
    def show(sym: Symbol)(implicit ctx: Context): String
    def showLocated(sym: Symbol)(implicit ctx: Context): String
    def showDef(sym: Symbol)(implicit ctx: Context): String
    def show(sc: Scope)(implicit ctx: Context): String
  }

  class StdPrinter extends Printer {
    def show(tp: Type)(implicit ctx: Context): String = ???
    def show(sym: Symbol)(implicit ctx: Context): String = ???
    def showLocated(sym: Symbol)(implicit ctx: Context): String = ???
    def showDef(sym: Symbol)(implicit ctx: Context): String = ???
    def show(sc: Scope)(implicit ctx: Context): String =
      sc.toList.map(_.showDef).mkString("Scope{\n  ", ";\n  ", "\n}")
  }


}