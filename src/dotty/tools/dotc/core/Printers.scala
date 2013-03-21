package dotty.tools.dotc
package core

import Types._, Symbols._, Contexts._, Scopes._, Names._, NameOps._, Flags._
import Constants._, Annotations._, StdNames._, Denotations._, Trees._
import util.Texts._
import java.lang.Integer.toOctalString
import scala.annotation.switch

trait Printers { this: Context =>

  import Printers._

  def printer = if (this.debug) plainPrinter else refinedPrinter

}

object Printers {

  class Precedence(val value: Int) extends AnyVal {
    def parenthesize(nested: Precedence)(text: Text) =
      if (nested.value < value) "(" ~ text ~ ")" else text
  }

  val DotPrec       = new Precedence(4)
  val AndPrec       = new Precedence(3)
  val OrPrec        = new Precedence(2)
  val WithPrec      = new Precedence(1)
  val LeftArrowPrec = new Precedence(1)
  val GlobalPrec    = new Precedence(0)

  abstract class Printer {

    /** The name, possibley with with namespace suffix if debugNames is set:
     * /L for local names, /V for other term names, /T for type names
     */
    def nameString(name: Name): String

    /** The name of the given symbol.
     *  If !settings.debug, the original name where
     *  expansions of operators are translated back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniqid, adds id.
     */
    def nameString(sym: Symbol): String

    /** The fully qualified name of the symbol */
    def fullNameString(sym: Symbol): String

    /** The kind of the symbol */
    def kindString(sym: Symbol): String

    /** Textual representation, including symbol's kind e.g., "class Foo", "method Bar".
     *  If hasMeaninglessName is true, uses the owner's name to disambiguate identity.
     */
    def toText(sym: Symbol): Text

    /** Textual representation of symbol's declaration */
    def dclText(sym: Symbol): Text

    /** If symbol's owner is a printable class C, the text "in C", otherwise "" */
    def locationText(sym: Symbol): Text

    /** Textual representation of symbol and its location */
    def locatedText(sym: Symbol): Text

    /** Textual representation of denotation */
    def toText(denot: Denotation): Text

    /** Textual representation of constant */
    def toText(const: Constant): Text

    /** Textual representation of annotation */
    def toText(annot: Annotation): Text

    /** Textual representation of type in context with given precedence */
    def toText(tp: Type, precedence: Precedence): Text

    /** Textual representation of all symbols in given list,
     *  using `dclText` for displaying each.
     */
    def dclsText(syms: List[Symbol], sep: String = "\n"): Text

    /** Textual representation of all definitions in a scope using `dclText` for each */
    def toText(sc: Scope): Text

    /** Textual representation of tree */
    def toText[T](tree: Tree[T]): Text
  }

  class PlainPrinter(_ctx: Context) extends Printer {
    protected[this] implicit val ctx = _ctx

    def controlled(op: => Text): Text =
      if (ctx.toTextRecursions < maxToTextRecursions)
        try {
          ctx.toTextRecursions += 1
          op
        } catch {
          case ex: CyclicReference =>
            "<cycle involving ${ex.denot}>"
        } finally {
          ctx.toTextRecursions -= 1
        }
      else {
        recursionLimitExceeeded()
        "..."
      }

    protected def recursionLimitExceeeded() = {
      ctx.warning("Exceeded recursion depth attempting to print type.")
      (new Throwable).printStackTrace
    }

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

    def nameString(name: Name): String = name.toString + {
      if (ctx.settings.debugNames.value)
        if (name.isLocalName) "/L"
        else if (name.isTypeName) "/T"
        else "/V"
      else ""
    }

    /** String representation of a name used in a refinement
     *  In refined printing this undoes type parameter expansion
     */
    protected def refinementNameString(tp: RefinedType) = nameString(tp.refinedName)

    /** String representation of a refinement */
    protected def toTextRefinement(rt: RefinedType) =
      (refinementNameString(rt) ~ toTextRHS(rt.refinedInfo)).close

    /** The longest sequence of refinement types, starting at given type
     *  and following parents.
     */
    private def refinementChain(tp: Type): List[Type] =
      tp :: (tp match {
        case RefinedType(parent, _) => refinementChain(parent)
        case _ => Nil
      })

    def toText(tp: Type, prec: Precedence): Text = controlled {
      tp match {
        case tp: TypeType =>
          toTextRHS(tp)
        case tp: SingletonType =>
          val pre = toTextPrefix(tp)
          if (pre.lastLine.endsWith(".")) pre ~ "type"
          else fullNameString(tp.typeSymbol.skipPackageObject) ~ ".type"
        case TypeRef(pre, name) =>
          toTextPrefix(pre) ~ nameString(tp.typeSymbol)
        case tp: RefinedType =>
          // return tp.toString // !!! DEBUG
          val parent :: (refined: List[RefinedType]) =
            refinementChain(tp).reverse
          toTextLocal(parent) ~ "{" ~
          Text(refined.map(toTextRefinement), "; ").close ~ "}"
        case AndType(tp1, tp2) =>
          (prec parenthesize AndPrec) {
            toText(tp1, AndPrec) ~ " & " ~ toText(tp2, AndPrec)
          }
        case OrType(tp1, tp2) =>
          (prec parenthesize OrPrec) {
            toText(tp1, OrPrec) ~ " | " ~ toText(tp2, OrPrec)
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
            (if (tp.isImplicit) "(implicit " else "(") ~
            Text(
              (tp.paramNames, tp.paramTypes).zipped
                .map((name, tp) => nameString(name) ~ ": " ~ toTextGlobal(tp)),
            ", ") ~
            ")" ~ toTextGlobal(tp.resultType)
          }
        case tp: ExprType =>
          (prec parenthesize GlobalPrec) {
            "=> " ~ toTextGlobal(tp.resultType)
          }
        case tp: PolyType =>
          (prec parenthesize GlobalPrec) {
            "[" ~
            Text(
              (tp.paramNames, tp.paramBounds).zipped
                .map((name, bounds) => nameString(name) ~ toTextGlobal(bounds)),
              ", ") ~
            "]" ~ toTextGlobal(tp.resultType)
          }
        case PolyParam(pt, n) =>
          nameString(pt.paramNames(n))
        case AnnotatedType(annot, tpe) =>
          toTextLocal(tpe) ~ " " ~ toText(annot)
      }
    }.close

    /** Render type within highest precedence */
    protected def toTextLocal(tp: Type) = toText(tp, DotPrec)

    /** Render type within lowest precedence */
    protected def toTextGlobal(tp: Type) = toText(tp, GlobalPrec)

    /** The name of the symbol without a unique id. Under refined printing,
     *  the decoded original name.
     */
    protected def simpleNameString(sym: Symbol): String = nameString(sym.name)

    /** The unique id of symbol, after a # */
    protected def idString(sym: Symbol): String =
      if (ctx.settings.uniqid.value) "#" + sym.id else ""

    def nameString(sym: Symbol): String = simpleNameString(sym) + idString(sym)

    def fullNameString(sym: Symbol): String =
      if (sym.isRoot || sym == NoSymbol || sym.owner.isEffectiveRoot)
        nameString(sym)
      else
        fullNameString(sym.effectiveOwner.enclosingClass) + "." + nameString(sym)

    protected def objectPrefix = "object "
    protected def packagePrefix = "package "

    protected def trimPrefix(text: Text) =
      text.stripPrefix(objectPrefix).stripPrefix(packagePrefix)

    /** The string representation of this type used as a prefix */
    protected def toTextPrefix(tp: Type): Text = controlled {
      tp match {
        case tp @ TermRef(pre, name) =>
          toTextPrefix(pre) ~ nameString(tp.symbol) ~ "."
        case ThisType(cls) =>
          nameString(cls) + ".this."
        case SuperType(thistpe, _) =>
          toTextPrefix(thistpe).map(_.replaceAll("""\bthis\.$""", "super."))
        case tp @ ConstantType(value) =>
          toTextLocal(tp.underlying) ~ "(" ~ toText(value) ~ ")."
        case MethodParam(mt, idx) =>
          nameString(mt.paramNames(idx)) + "."
        case RefinedThis(_) =>
          "this."
        case NoPrefix =>
          ""
        case _ =>
          trimPrefix(toTextLocal(tp)) ~ "#"
      }
    }

    protected def isOmittablePrefix(sym: Symbol) =
      (defn.UnqualifiedOwners contains sym) || isEmptyPrefix(sym)

    protected def isEmptyPrefix(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName

    /** String representation of a definition's type following its name */
    protected def toTextRHS(tp: Type): Text = controlled {
      tp match {
        case TypeBounds(lo, hi) =>
          if (lo eq hi)
            " = " ~ lo.toText
          else
            (if (lo == defn.NothingType) Text() else " >: " ~ lo.toText) ~
            (if (hi == defn.AnyType)     Text() else " <: " ~ hi.toText)
        case ClassInfo(pre, cdenot, cparents, decls, optSelfType) =>
          val preText = toTextLocal(pre)
          val selfText =
            if (optSelfType.exists)
              "this: " ~ toText(optSelfType, LeftArrowPrec) ~ " =>"
            else Text()
          val parentsText = Text(cparents.map(toText(_, WithPrec)), " with ")
          val declsText = if (decls.isEmpty) Text() else dclsText(decls.toList)
          " extends " ~ parentsText ~ "{" ~ selfText ~ declsText ~
          "} at " ~ preText
        case _ =>
          ": " ~ toTextGlobal(tp)
      }
    }

    /** String representation of symbol's kind. */
    def kindString(sym: Symbol): String =
      if (sym isUnsafe PackageClass) "package class"
      else if (sym isUnsafe PackageVal) "package"
      else if (sym.isPackageObject)
        if (sym.isClass) "package object class"
        else "package object"
      else if (sym.isAnonymousClass) "anonymous class"
      else if (sym isUnsafe ModuleClass) "module class"
      else if (sym isUnsafe ModuleVal) "module"
      else if (sym isUnsafe ImplClass) "implementation class"
      else if (sym isUnsafe Trait) "trait"
      else if (sym.isClass) "class"
      else if (sym.isType) "type"
      else if (sym.isGetter) "getter"
      else if (sym.isSetter) "setter"
      else if (sym isUnsafe Lazy) "lazy value"
      else if (sym isUnsafe Mutable) "variable"
      else if (sym.isClassConstructor && sym.isPrimaryConstructor) "primary constructor"
      else if (sym.isClassConstructor) "constructor"
      else if (sym.isSourceMethod) "method"
      else if (sym.isTerm) "value"
      else ""

    /** String representation of symbol's definition key word */
    protected def keyString(sym: Symbol): String =
      if (sym isUnsafe JavaInterface) "interface"
      else if ((sym isUnsafe Trait) && !(sym isUnsafe ImplClass)) "trait"
      else if (sym.isClass) "class"
      else if (sym.isType && !(sym isUnsafe ExpandedTypeParam)) "type"
      else if (sym isUnsafe Mutable) "var"
      else if (sym isUnsafe Package) "package"
      else if (sym isUnsafe Module) "object"
      else if (sym.isSourceMethod) "def"
      else if (sym.isTerm && (!(sym isUnsafe Param))) "val"
      else ""

    /** String representation of symbol's flags */
    protected def toTextFlags(sym: Symbol): Text =
      Text(sym.flags.flagStrings map stringToText, " ")

    /** String representation of symbol's variance or "" if not applicable */
    protected def varianceString(sym: Symbol): String = sym.variance match {
      case -1 => "-"
      case 1 => "+"
      case _ => ""
    }

    def dclText(sym: Symbol): Text =
      (toTextFlags(sym) ~~ keyString(sym) ~~
       (varianceString(sym) ~ nameString(sym)) ~ toTextRHS(sym.info)).close

    def toText(sym: Symbol): Text =
      (kindString(sym) ~~ {
        if (hasMeaninglessName(sym)) simpleNameString(sym.owner) + idString(sym)
        else nameString(sym)
      }).close

    def locationText(sym: Symbol): Text = {
      val owns = sym.effectiveOwner
      if (owns.isClass && !isEmptyPrefix(owns)) "in " ~ toText(owns) else Text()
    }

    def locatedText(sym: Symbol): Text =
      (toText(sym) ~ locationText(sym)).close

    def toText(denot: Denotation): Text = toText(denot.symbol) ~ "/D"

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

    def toText(const: Constant): Text = const.tag match {
        case StringTag => "\"" + (const.value.toString flatMap escapedChar) + "\""
        case ClazzTag  => "classOf[" ~ const.tpe.toText ~ "]"
        case CharTag   => s"'${escapedChar(const.charValue)}'"
        case LongTag   => const.longValue.toString + "L"
        case EnumTag   => const.symbolValue.name.toString
        case _         => String.valueOf(const.value)
    }

    def toText(annot: Annotation): Text = s"@${annot.symbol.name}" // for now

    def dclsText(syms: List[Symbol], sep: String): Text =
      if (sep == "\n") Text.lines(syms map dclText)
      else Text(syms map dclText, sep)

    def toText(sc: Scope): Text =
      ("Scope{" ~ dclsText(sc.toList) ~ "}").close

    def toText[T](tree: Tree[T]): Text = {
      tree match {
        case node: Product =>
          def toTextElem(elem: Any): Text = elem match {
            case elem: Showable => elem.toText
            case elem => elem.toString
          }
          val nodeName = node.productPrefix
          val elems =
            Text(node.productIterator.map(toTextElem).toList, ", ")
          val tpSuffix =
            if (ctx.settings.printtypes.value && tree.hasType)
              " | " ~ tree.tpe.asInstanceOf[Type].toText
            else
              Text()

          nodeName ~ "(" ~ elems ~ tpSuffix ~ ")"
        case _ =>
          tree.toString: Text
      }
    }.close // todo: override in refined printer
  }

  class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {
    override protected def recursionLimitExceeeded() = {}

    override def nameString(name: Name): String = name.toString

    override protected def simpleNameString(sym: Symbol) = sym.originalName.decode

    override def toTextPrefix(tp: Type): Text = controlled {
      tp match {
        case ThisType(cls) =>
          if (cls.isAnonymousClass) return "this."
          if (isOmittablePrefix(cls)) return ""
          if (cls is ModuleClass) return fullNameString(cls) + "."
        case tp @ TermRef(pre, name) =>
          val sym = tp.symbol
          if (sym.isPackageObject) return toTextPrefix(pre)
          if (isOmittablePrefix(sym)) return ""
        case _ =>
      }
      super.toTextPrefix(tp)
    }

    override protected def refinementNameString(tp: RefinedType): String = {
      val tsym = tp.member(tp.refinedName).symbol
      val name = tsym.originalName
      nameString(if (tsym is ExpandedTypeParam) name.asTypeName.unexpandedName() else name)
    }

    override def toText(tp: Type, prec: Precedence): Text = controlled {
      def toTextFunction(args: List[Type]): Text =
        (prec parenthesize GlobalPrec) {
          val argStr: Text =
            if (args.length == 2 &&
              !(defn.TupleClasses contains args.head.typeSymbol)) toText(args.head, LeftArrowPrec)
            else
              "(" ~ Text(args.init.map(toTextGlobal(_)), ", ") ~ ")"
          argStr ~ " => " ~ toTextGlobal(args.last)
        }
      def toTextTuple(args: List[Type]): Text =
        "(" ~ Text(args.map(toTextGlobal(_)), ", ") ~ ")"
      try {
        tp match {
          case tp: RefinedType =>
            val args = tp.typeArgs
            if (args.nonEmpty) {
              val tycon = tp.unrefine
              val cls = tycon.typeSymbol
              if (cls.typeParams.length == args.length) {
                if (cls == defn.RepeatedParamClass) return toTextLocal(args.head) ~ "*"
                if (cls == defn.ByNameParamClass) return "=> " ~ toTextGlobal(args.head)
                if (defn.FunctionClasses contains cls) return toTextFunction(args)
                if (defn.TupleClasses contains cls) return toTextTuple(args)
              }
              return toTextLocal(tycon) ~ "[" ~
                     Text(args.map(toTextGlobal(_)), ", ") ~ "]"
            }
          case _ =>
        }
      } catch {
        case ex: CyclicReference =>
          "<cylic reference during display>" ~ super.toText(tp, prec)
      }
      super.toText(tp, prec)
    }

    override def kindString(sym: Symbol) =
      if (sym isUnsafe Package) "package"
      else if (sym.isPackageObject) "package object"
      else if (sym isUnsafe Module) "object"
      else if (sym isUnsafe ImplClass) "class"
      else if (sym.isClassConstructor) "constructor"
      else super.kindString(sym)

    override def toTextFlags(sym: Symbol) =
      Text(sym.flags.flagStrings.filterNot(_.startsWith("<")) map stringToText, " ")

    override def toText(denot: Denotation): Text = toText(denot.symbol)
  }

  final val maxToTextRecursions = 100

}
