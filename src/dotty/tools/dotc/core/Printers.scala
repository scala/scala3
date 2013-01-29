package dotty.tools.dotc
package core

import Types._, Symbols._, Contexts._, Scopes._

trait Printers {

  private[this] var _diagnostics: Option[StringBuilder] = _
  protected def diagnostics_=(diagnostics: Option[StringBuilder]) = _diagnostics = diagnostics
  def diagnostics: Option[StringBuilder] = _diagnostics

  def diagnose(str: => String) =
    for (sb <- diagnostics) {
      sb.setLength(0)
      sb.append(str)
    }
}


object Printers {

  trait PrinterBase { self: ContextBase =>

    private[core] var showRecursions = 0

  }

  abstract class Printer {
    def show(tp: Type)(implicit ctx: Context): String
    def show(sym: Symbol)(implicit ctx: Context): String
    def showLocated(sym: Symbol)(implicit ctx: Context): String
    def showDef(sym: Symbol)(implicit ctx: Context): String
    def show(sc: Scope)(implicit ctx: Context): String
  }

  object StdPrinter {
    final val maxShowRecursions = 50
  }

  class StdPrinter extends Printer {
    import StdPrinter._

    def controlled(op: => String)(implicit ctx: Context): String =
      if (ctx.showRecursions < maxShowRecursions)
        try {
          ctx.showRecursions += 1
          op
        } finally {
          ctx.showRecursions -= 1
        }
      else {
        if (???/*ctx.settings.debug.value*/) {
          ctx.warning("Exceeded recursion depth attempting to print type.")
          (new Throwable).printStackTrace
        }
        "..."
      }

    def show(tp: Type)(implicit ctx: Context): String = controlled {
      tp match {
        case TermRef(pre, name) =>
          ??? // showPrefix(pre) + show(name)



      }
    }
    def show(sym: Symbol)(implicit ctx: Context): String = controlled {
      ???
    }
    def showLocated(sym: Symbol)(implicit ctx: Context): String = ???
    def showDef(sym: Symbol)(implicit ctx: Context): String = ???
    def show(sc: Scope)(implicit ctx: Context): String =
      sc.toList.map(_.showDef).mkString("Scope{\n  ", ";\n  ", "\n}")
  }



}