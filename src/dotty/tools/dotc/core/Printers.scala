package dotty.tools.dotc
package core

import Types._, Symbols._, Contexts._, Scopes._, Names._, NameOps._, Flags._

trait Printers { this: Context =>

  def show(tp: Type): String = printer(this).show(tp)
  def show(sym: Symbol): String = printer(this).show(sym)
  def showLocated(sym: Symbol): String = printer(this).showLocated(sym)
  def showDef(sym: Symbol): String = printer(this).showDef(sym)
  def show(sc: Scope): String = printer(this).show(sc)
  def show(syms: List[Symbol], sep: String): String = printer(this).show(syms, sep)
  def showNameDetailed(name: Name) = printer(this).showNameDetailed(name)

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
    def show(syms: List[Symbol], sep: String): String
    def showNameDetailed(name: Name): String
    def showFullName(sym: Symbol): String

    /** String representation of symbol's simple name.
     *  If !settings.debug translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniqid, adds id.
     *  If settings.Yshowsymkinds, adds abbreviated symbol kind.
     */
    def showName(sym: Symbol): String
  }

  class PlainPrinter(_ctx: Context) extends Printer {
    protected[this] implicit val ctx = _ctx
    def controlled(op: => String): String =
      if (ctx.showRecursions < maxShowRecursions)
        try {
          ctx.showRecursions += 1
          op
        } finally {
          ctx.showRecursions -= 1
        }
      else {
        recursionLimitExceeeded()
        "..."
      }

    protected def recursionLimitExceeeded() = {
      ctx.warning("Exceeded recursion depth attempting to print type.")
      (new Throwable).printStackTrace
    }

    protected def showSimpleName(sym: Symbol) = sym.originalName.decode

    def showName(sym: Symbol): String =
      showSimpleName(sym) + (if (ctx.settings.uniqid.value) "#" + sym.id else "")

    def showFullName(sym: Symbol): String =
      if (sym.isRoot || sym == NoSymbol || sym.owner.isEffectiveRoot)
        showName(sym)
      else
        showFullName(sym.effectiveOwner.enclosingClass) + "." + showName(sym)

    protected def objectPrefix = "object "
    protected def packagePrefix = "package "

    protected def trimPrefix(str: String) =
      str.stripPrefix(objectPrefix).stripPrefix(packagePrefix)

    protected def isOmittablePrefix(sym: Symbol) =
      (defn.UnqualifiedOwners contains sym) || isEmptyPrefix(sym)

    protected  def isEmptyPrefix(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName

    def showPrefix(tp: Type): String = controlled {
      tp match {
        case RefinedThis(_) =>
          "this."
        case ThisType(cls) =>
          showName(cls) + ".this."
        case SuperType(thistpe, _) =>
          showPrefix(thistpe).replaceAll("""\bthis\.$""", "super.")
        case tp @ TermRef(pre, name) =>
          val sym = tp.symbol
          if (!sym.exists) showPrefix(pre) + name + "."
          else showPrefix(pre) + showName(sym) + "."
        case MethodParam(mt, idx) =>
          mt.paramNames(idx) + "."
        case NoPrefix =>
          ""
        case ConstantType(value) =>
          "(" + value + ")."
        case _ =>
          trimPrefix(show(tp)) + "#"
      }
    }

    def show(tp: Type): String = controlled {
      tp match {
        case tp: SingletonType =>
          val str = showPrefix(tp)
          if (str.endsWith(".")) str + "type"
          else showFullName(tp.underlying.typeSymbol.skipPackageObject) + ".type"
        case
          TermRef(pre, name) =>
          ??? // showPrefix(pre) + show(name)



      }
    }


    def show(sym: Symbol): String = controlled {

      ???
    }
    def showLocated(sym: Symbol): String = ???
    def showDef(sym: Symbol): String = ???
    def show(sc: Scope): String =
      "Scope{\n" + show(sc.toList, ";\n  ") + "\n}"

    def show(syms: List[Symbol], sep: String): String =
      syms map (_.showDef) mkString sep

    def showNameDetailed(name: Name): String =
      (if (name.isTypeName) "type " else "term ") + name
  }

  class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {
    override protected def recursionLimitExceeeded() = {}

    override protected def showSimpleName(sym: Symbol) = sym.name.toString

    override def showPrefix(tp: Type): String = controlled {
      tp match {
        case ThisType(cls) =>
          if (cls.isAnonymousClass) return "this."
          if (isOmittablePrefix(cls)) return ""
          if (cls.isModuleClass) return showFullName(cls) + "."
        case tp @ TermRef(pre, name) =>
          val sym = tp.symbol
          if (sym is PackageObject) return showPrefix(pre)
          if (isOmittablePrefix(sym)) return ""
        case _ =>
      }
      super.showPrefix(tp)
    }
  }

  final val maxShowRecursions = 100

}