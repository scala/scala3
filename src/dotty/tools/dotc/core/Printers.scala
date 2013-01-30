package dotty.tools.dotc
package core

import Types._, Symbols._, Contexts._, Scopes._

trait Printers { this: Context =>

  def show(tp: Type): String = printer(this).show(tp)
  def show(sym: Symbol): String = printer(this).show(sym)
  def showLocated(sym: Symbol): String = printer(this).showLocated(sym)
  def showDef(sym: Symbol): String = printer(this).showDef(sym)
  def show(sc: Scope): String = printer(this).show(sc)

  private var _diagnostics: Option[StringBuilder] = _

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
    def show(tp: Type): String
    def show(sym: Symbol): String
    def showLocated(sym: Symbol): String
    def showDef(sym: Symbol): String
    def show(sc: Scope): String
  }

  class StdPrinter(implicit ctx: Context) extends Printer {

    def controlled(op: => String): String =
      if (ctx.showRecursions < maxShowRecursions)
        try {
          ctx.showRecursions += 1
          op
        } finally {
          ctx.showRecursions -= 1
        }
      else {
        if (ctx.debug) {
          ctx.warning("Exceeded recursion depth attempting to print type.")
          (new Throwable).printStackTrace
        }
        "..."
      }

    def show(tp: Type): String = controlled {
      tp match {
        case TermRef(pre, name) =>
          ??? // showPrefix(pre) + show(name)



      }
    }

    protected def objectPrefix = "object "
    protected def packagePrefix = "package "

    protected def trimPrefix(str: String) =
      str.stripPrefix(objectPrefix).stripPrefix(packagePrefix)

    protected def isOmittablePrefix(sym: Symbol) =
      (defn.UnqualifiedOwners contains sym) || isEmptyPrefix(sym)

    protected  def isEmptyPrefix(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || ??? // nme.isReplWrapperName(sym.name)


    def showPrefix(tp: Type): String = controlled {
      tp match {
        case ThisType(cls) =>
          if (ctx.debug) showName(cls) + ".this."
          else if (cls.isAnonymousClass) "this."
          else ???
        case NoPrefix =>
          ""
        case _ =>
          trimPrefix(show(tp)) + "#"

      }
    }

    def show(sym: Symbol): String = controlled {

      ???
    }
    def showName(sym: Symbol): String = ???
    def showLocated(sym: Symbol): String = ???
    def showDef(sym: Symbol): String = ???
    def show(sc: Scope): String =
      sc.toList.map(_.showDef).mkString("Scope{\n  ", ";\n  ", "\n}")
  }

  final val maxShowRecursions = 50

}