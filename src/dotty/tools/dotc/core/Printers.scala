package dotty.tools.dotc
package core

import Types._, Symbols._, Contexts._, Scopes._, Names._, NameOps._, Flags._
import Constants._, Annotations._, StdNames._
import java.lang.Integer.toOctalString
import scala.annotation.switch

trait Printers { this: Context =>

  import Printers._

  def show(tp: Type): String = printer(this).show(tp, GlobalPrec)

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

  class Precedence(val value: Int) extends AnyVal {
    def parenthesize(nested: Precedence)(str: String) =
      if (nested.value < value) "(" + str + ")" else str
  }

  val DotPrec       = new Precedence(4)
  val AndPrec       = new Precedence(3)
  val OrPrec        = new Precedence(2)
  val WithPrec      = new Precedence(1)
  val LeftArrowPrec = new Precedence(1)
  val GlobalPrec    = new Precedence(0)

  trait PrinterBase { self: ContextBase =>
    private[core] var showRecursions = 0
  }

  abstract class Printer {

    /** Show name, same as name.toString */
    def show(name: Name): String

    /** Show name with "type" or "term" prefix */
    def showDetailed(name: Name): String

    /** Show type in context with given precedence */
    def show(tp: Type, precedence: Precedence): String

    /** Show name of symbol.
     *  If !settings.debug shows original name and
     *  translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniqid, adds id.
     */
    def showName(sym: Symbol): String

    /** Show fully qualified name of symbol */
    def showFullName(sym: Symbol): String

    /** Show kind of symbol */
    def showKind(sym: Symbol): String

    /** String representation, including symbol's kind e.g., "class Foo", "method Bar".
     *  If hasMeaninglessName is true, uses the owner's name to disambiguate identity.
     */
    def show(sym: Symbol): String

    /** Show symbol's declaration */
    def showDcl(sym: Symbol): String

    /** If symbol's owner is printable class C, the string "in C", otherwise "" */
    def showLocation(sym: Symbol): String

    /** Show symbol and its location */
    def showLocated(sym: Symbol): String

    /** Show constant */
    def show(const: Constant)

    /** Show annotation */
    def show(annot: Annotation): String

    /** Show all symbols in given list separated by `sep`, using `showDcl` for each */
    def show(syms: List[Symbol], sep: String): String

    /** Show all definitions in a scope usng `showDcl` for each */
    def show(sc: Scope): String
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

    /** Concatenate strings separated by spaces */
    protected def compose(ss: String*) = ss filter (_.nonEmpty) mkString " "

    /** If the name of the symbol's owner should be used when you care about
     *  seeing an interesting name: in such cases this symbol is e.g. a method
     *  parameter with a synthetic name, a constructor named "this", an object
     *  "package", etc.  The kind string, if non-empty, will be phrased relative
     *  to the name of the owner.
     */
    protected def hasMeaninglessName(sym: Symbol) = (
         (sym is Param) && sym.owner.isSetter    // x$1
      || sym.isClassConstructor                  // this
      || (sym.name == nme.PACKAGE)               // package
    )

    def show(name: Name): String = name.toString

    def showDetailed(name: Name): String =
      (if (name.isTypeName) "type " else "term ") + name

    /** String representation of a name used in a refinement
     *  In refined printing this undoes type parameter expansion
     */
    protected def showRefinementName(tp: RefinedType) = show(tp.name)

    /** String representation of a refinement */
    protected def showRefinement(rt: RefinedType) =
      showRefinementName(rt) + showRHS(rt.info)

    /** The longest sequence refinement types, starting at given type
     *  and following parents.
     */
    private def refinementChain(tp: Type): List[Type] =
      tp :: (tp match {
        case RefinedType(parent, _) => refinementChain(parent)
        case _ => Nil
      })

    def show(tp: Type, prec: Precedence): String = controlled {
      tp match {
        case tp: TypeType =>
          showRHS(tp)
        case tp: SingletonType =>
          val str = showPrefix(tp)
          if (str.endsWith(".")) str + "type"
          else showFullName(tp.underlying.typeSymbol.skipPackageObject) + ".type"
        case TypeRef(pre, name) =>
          showPrefix(pre) + showName(tp.typeSymbol)
        case tp: RefinedType =>
          val parent :: refined = refinementChain(tp).reverse
          showLocal(parent) +
          refined.asInstanceOf[List[RefinedType]].map(showRefinement).mkString("{", "; ", "}")
        case AndType(tp1, tp2) =>
          (prec parenthesize AndPrec) {
            show(tp1, AndPrec) + "&" + show(tp2, AndPrec)
          }
        case OrType(tp1, tp2) =>
          (prec parenthesize OrPrec) {
            show(tp1, OrPrec) + "|" + show(tp2, OrPrec)
          }
        case ErrorType =>
          "<error>"
        case WildcardType =>
          "?"
        case NoType =>
          "<notype>"
        case NoPrefix =>
          "<noprefix>"
        case tp: MethodType =>
          (prec parenthesize GlobalPrec) {
            val openStr = if (tp.isImplicit) "(implicit " else "("
              (tp.paramNames, tp.paramTypes).zipped
              .map((name, tp) => show(name) + ": " + showGlobal(tp))
              .mkString(openStr, ", ", ")" + showGlobal(tp.resultType))
          }
        case tp: ExprType =>
          (prec parenthesize GlobalPrec) {
            "=> " + showGlobal(tp.resultType)
          }
        case tp: PolyType =>
          (prec parenthesize GlobalPrec) {
            (tp.paramNames, tp.paramBounds).zipped
              .map((name, bounds) => show(name) + showGlobal(bounds))
              .mkString("[", ", ", "]" + showGlobal(tp.resultType))
          }
        case PolyParam(pt, n) =>
          show(pt.paramNames(n))
        case AnnotatedType(annots, tpe) =>
          showLocal(tpe) + " " + annots.map(show).mkString(" ")
      }
    }

    /** Show type within highest precedence */
    protected def showLocal(tp: Type) = show(tp, DotPrec)

    /** Show type within lowest precedence */
    protected def showGlobal(tp: Type) = show(tp, GlobalPrec)

    /** Show name of symbol without unique id. Under refined printing,
     *  shows decoded original name.
     */
    protected def showSimpleName(sym: Symbol) = show(sym.name)

    /** Show unique id of symbol, after a # */
    protected def showId(sym: Symbol) =
      if (ctx.settings.uniqid.value) "#" + sym.id else ""

    def showName(sym: Symbol): String = showSimpleName(sym) + showId(sym)

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

    protected def isEmptyPrefix(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName

    @switch private def escapedChar(ch: Char): String = ch match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"' => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case _ => if (ch.isControl) "\\0" + toOctalString(ch) else String.valueOf(ch)
    }

    /** The string representation of this type used as a prefix */
    protected def showPrefix(tp: Type): String = controlled {
      tp match {
        case tp @ TermRef(pre, name) =>
          showPrefix(pre) + showName(tp.symbol) + "."
        case ThisType(cls) =>
          showName(cls) + ".this."
        case SuperType(thistpe, _) =>
          showPrefix(thistpe).replaceAll("""\bthis\.$""", "super.")
        case tp @ ConstantType(value) =>
          showLocal(tp.underlying) + "(" + show(value) + ")."
        case MethodParam(mt, idx) =>
          show(mt.paramNames(idx)) + "."
        case RefinedThis(_) =>
          "this."
        case NoPrefix =>
          ""
        case _ =>
          trimPrefix(showLocal(tp)) + "#"
      }
    }

    /** String representation of a definition's type following its name */
    protected def showRHS(tp: Type): String = controlled {
      tp match {
        case TypeBounds(lo, hi) =>
          if (lo eq hi)
            " = " + lo
          else
            (if (lo.typeSymbol == defn.NothingClass) "" else ">: " + lo) +
            (if (hi.typeSymbol == defn.AnyClass)     "" else "<: " + hi)
        case ClassInfo(pre, cdenot) =>
          val preStr = showLocal(pre)
          val selfStr =
            if (cdenot.selfType == cdenot.typeConstructor) ""
            else s"this: ${show(cdenot.selfType, LeftArrowPrec)} =>"
          val parentsStr = cdenot.parents.map(show(_, WithPrec)).mkString(" with ")
          val declsStr =
            if (cdenot.decls.isEmpty) ""
            else "\n  " + show(cdenot.decls.toList, "\n  ")
          s"""$parentsStr { $selfStr$declsStr
             |} at $preStr""".stripMargin
        case _ => ": " + showGlobal(tp)
      }
    }

    /** Show kind of symbol */
    def showKind(sym: Symbol) =
      if (sym.isPackageClass) "package class"
      else if (sym.isPackageVal) "package"
      else if (sym is PackageObjectClass) "package object class"
      else if (sym is PackageObjectVal) "package object"
      else if (sym.isAnonymousClass) "anonymous class"
      else if (sym.isModuleClass) "module class"
      else if (sym.isModuleVal) "module"
      else if (sym is ImplClass) "implementation class"
      else if (sym is Trait) "trait"
      else if (sym.isClass) "class"
      else if (sym.isType) "type"
      else if (sym.isGetter) "getter"
      else if (sym.isSetter) "setter"
      else if (sym is Lazy) "lazy value"
      else if (sym is Mutable) "variable"
      else if (sym.isClassConstructor && sym.isPrimaryConstructor) "primary constructor"
      else if (sym.isClassConstructor) "constructor"
      else if (sym.isSourceMethod) "method"
      else if (sym.isTerm) "value"
      else ""

    /** String representation of symbol's definition key word */
    protected def showKey(sym: Symbol): String =
      if (sym is JavaInterface) "interface"
      else if (sym is (Trait, butNot = ImplClass)) "trait"
      else if (sym.isClass) "class"
      else if (sym.isType && !(sym is ExpandedTypeParam)) "type"
      else if (sym is Mutable) "var"
      else if (sym.isPackage) "package"
      else if (sym.isModule) "object"
      else if (sym.isSourceMethod) "def"
      else if (sym.isTerm && (!(sym is Param))) "val"
      else ""

    /** String representation of symbol's flags */
    protected def showFlags(sym: Symbol) = sym.flags.toString

    /** String representation of symbol's variance or "" if not applicable */
    protected def showVariance(sym: Symbol) = sym.variance match {
      case -1 => "-"
      case 1 => "+"
      case _ => ""
    }

    def showDcl(sym: Symbol): String = compose(
      showFlags(sym),
      showKey(sym),
      showVariance(sym) + showName(sym) + showRHS(sym.info))

    def show(sym: Symbol): String = compose(
      showKind(sym),
      if (hasMeaninglessName(sym)) showSimpleName(sym.owner) + showId(sym)
      else showName(sym)
    )

    def showLocation(sym: Symbol): String = {
      val owns = sym.effectiveOwner
      if (owns.isClass && !isEmptyPrefix(owns)) "in "+show(owns) else ""
    }

    def showLocated(sym: Symbol): String =
      show(sym) + showLocation(sym)

    def show(const: Constant) = {
      ??? /*
      def escape(text: String): String = text flatMap escapedChar
      tag match {
        case NullTag   => "null"
        case StringTag => "\"" + escape(stringValue) + "\""
        case ClazzTag  =>
          def show(tpe: Type) = "classOf[" + signature(tpe) + "]"
          typeValue match {
            case ErasedValueType(orig) => show(orig)
            case _ => show(typeValue)
          }
        case CharTag   => "'" + escapedChar(charValue) + "'"
        case LongTag   => longValue.toString() + "L"
        case EnumTag   => symbolValue.name.toString()
        case _         => String.valueOf(value)
      }*/
    }

    def show(annot: Annotation): String = ???

    def show(syms: List[Symbol], sep: String): String =
      syms map (_.showDcl) mkString sep

    def show(sc: Scope): String =
      "Scope{\n" + show(sc.toList, ";\n  ") + "\n}"

  }

  class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {
    override protected def recursionLimitExceeeded() = {}

    override protected def showSimpleName(sym: Symbol) = sym.originalName.decode

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

    override protected def showRefinementName(tp: RefinedType): String = {
      val tsym = tp.member(tp.name).symbol
      val name = tsym.originalName
      show(if (tsym is ExpandedTypeParam) name.asTypeName.unexpandedName() else name)
    }

    override def show(tp: Type, prec: Precedence): String = controlled {
      def showFunction(args: List[Type]): String =
        (prec parenthesize GlobalPrec) {
          val argStr =
            if (args.length == 2 &&
              !(defn.TupleClasses contains args.head.typeSymbol)) show(args.head, LeftArrowPrec)
            else args.init.map(showGlobal(_)).mkString("(", ", ", ")")
          argStr + " => " + showGlobal(args.last)
        }
      def showTuple(args: List[Type]): String =
        args.map(showGlobal(_)).mkString("(", ", ", ")")
      try {
        tp match {
          case tp: RefinedType =>
            val (tycon, args) = tp.splitArgs
            if (args.nonEmpty) {
              if (tycon.typeParams.length == args.length) {
                val cls = tycon.typeSymbol
                if (cls == defn.RepeatedParamClass) return showLocal(args.head) + "*"
                if (cls == defn.ByNameParamClass) return "=> " + showGlobal(args.head)
                if (defn.FunctionClasses contains cls) return showFunction(args)
                if (defn.TupleClasses contains cls) return showTuple(args)
              }
              return showLocal(tycon) + args.map(showGlobal(_)).mkString("[", ", ", "]")
            }
          case _ =>
        }
      } catch {
        case ex: CyclicReference =>
          "<cylic reference during display>" + super.show(tp, prec)
      }
      super.show(tp, prec)
    }

    override def showKind(sym: Symbol) =
      if (sym.isPackage) "package"
      else if (sym is PackageObject) "package object"
      else if (sym is Module) "object"
      else if (sym is ImplClass) "class"
      else if (sym.isClassConstructor) "constructor"
      else super.showKind(sym)

    override def showFlags(sym: Symbol) =
      sym.flags.flagStrings.filterNot(_.startsWith("<")).mkString(" ")
  }

  final val maxShowRecursions = 100

}
