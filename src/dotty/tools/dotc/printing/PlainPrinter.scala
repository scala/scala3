package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Constants._, Denotations._
import Contexts.Context, Scopes.Scope, Denotations.Denotation, Annotations.Annotation
import StdNames.{nme, tpnme}
import ast.Trees._, ast._
import java.lang.Integer.toOctalString
import config.Config.summarizeDepth
import scala.annotation.switch

class PlainPrinter(_ctx: Context) extends Printer {
  protected[this] implicit def ctx: Context = _ctx.addMode(Mode.Printing)

  protected def maxToTextRecursions = 100

  protected final def controlled(op: => Text): Text =
    if (ctx.toTextRecursions < maxToTextRecursions && ctx.toTextRecursions < maxSummarized)
      try {
        ctx.toTextRecursions += 1
        op
      } finally {
        ctx.toTextRecursions -= 1
      }
    else {
      if (ctx.toTextRecursions >= maxToTextRecursions)
        recursionLimitExceeded()
      "..."
    }

  protected def recursionLimitExceeded() = {
    ctx.warning("Exceeded recursion depth attempting to print.")
    if (ctx.debug) Thread.dumpStack()
  }

  /** If true, tweak output so it is the same before and after pickling */
  protected def homogenizedView: Boolean = ctx.settings.YtestPickler.value

  def homogenize(tp: Type): Type =
    if (homogenizedView)
      tp match {
        case tp: ThisType if tp.cls.is(Package) && !tp.cls.isEffectiveRoot =>
          ctx.requiredPackage(tp.cls.fullName).termRef
        case tp: TypeVar if tp.isInstantiated =>
          homogenize(tp.instanceOpt)
        case AndType(tp1, tp2) =>
          homogenize(tp1) & homogenize(tp2)
        case OrType(tp1, tp2) =>
          homogenize(tp1) | homogenize(tp2)
        case tp @ TypeRef(_, tpnme.hkApply) =>
          val tp1 = tp.reduceProjection
          if (tp1 eq tp) tp else homogenize(tp1)
        case tp: LazyRef =>
          homogenize(tp.ref)
        case _ =>
          tp
      }
    else tp

  /** Render elements alternating with `sep` string */
  protected def toText(elems: Traversable[Showable], sep: String) =
    Text(elems map (_ toText this), sep)

  /** Render element within highest precedence */
  protected def toTextLocal(elem: Showable): Text =
    atPrec(DotPrec) { elem.toText(this) }

  /** Render element within lowest precedence */
  protected def toTextGlobal(elem: Showable): Text =
    atPrec(GlobalPrec) { elem.toText(this) }

  protected def toTextLocal(elems: Traversable[Showable], sep: String) =
    atPrec(DotPrec) { toText(elems, sep) }

  protected def toTextGlobal(elems: Traversable[Showable], sep: String) =
    atPrec(GlobalPrec) { toText(elems, sep) }

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
      if (name.isTypeName) "/T" else "/V"
    else ""
  }

  def toText(name: Name): Text = Str(nameString(name))

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
      case tp: RefinedType => refinementChain(tp.parent.stripTypeVar)
      case _ => Nil
    })

  def toText(tp: Type): Text = controlled {
    homogenize(tp) match {
      case tp: TypeType =>
        toTextRHS(tp)
      case tp: TermRef if !tp.denotationIsCurrent || tp.symbol.is(Module) || tp.symbol.name.isImportName =>
        toTextRef(tp) ~ ".type"
      case tp: TermRef if tp.denot.isOverloaded =>
        "<overloaded " ~ toTextRef(tp) ~ ">"
      case tp: SingletonType =>
        toTextLocal(tp.underlying) ~ "(" ~ toTextRef(tp) ~ ")"
      case tp: TypeRef =>
        toTextPrefix(tp.prefix) ~ selectionString(tp)
      case tp: RefinedType =>
        val parent :: (refined: List[RefinedType @unchecked]) =
          refinementChain(tp).reverse
        toTextLocal(parent) ~ "{" ~ Text(refined map toTextRefinement, "; ").close ~ "}"
      case AndType(tp1, tp2) =>
        changePrec(AndPrec) { toText(tp1) ~ " & " ~ toText(tp2) }
      case OrType(tp1, tp2) =>
        changePrec(OrPrec) { toText(tp1) ~ " | " ~ toText(tp2) }
      case ErrorType =>
        "<error>"
      case tp: WildcardType =>
        if (tp.optBounds.exists) "(?" ~ toTextRHS(tp.bounds) ~ ")" else "?"
      case NoType =>
        "<notype>"
      case NoPrefix =>
        "<noprefix>"
      case tp: MethodType =>
        def paramText(name: TermName, tp: Type) = toText(name) ~ ": " ~ toText(tp)
        changePrec(GlobalPrec) {
          (if (tp.isImplicit) "(implicit " else "(") ~
            Text((tp.paramNames, tp.paramTypes).zipped map paramText, ", ") ~
          ")" ~ toText(tp.resultType)
        }
      case tp: ExprType =>
        changePrec(GlobalPrec) { "=> " ~ toText(tp.resultType) }
      case tp: PolyType =>
        def paramText(name: TypeName, bounds: TypeBounds) =
          toText(polyParamName(name)) ~ polyHash(tp) ~ toText(bounds)
        changePrec(GlobalPrec) {
          "[" ~
            Text((tp.paramNames, tp.paramBounds).zipped map paramText, ", ") ~
          "]" ~ toText(tp.resultType)
        }
      case PolyParam(pt, n) =>
        toText(polyParamName(pt.paramNames(n))) ~ polyHash(pt)
      case AnnotatedType(tpe, annot) =>
        toTextLocal(tpe) ~ " " ~ toText(annot)
      case tp: TypeVar =>
        if (tp.isInstantiated)
          toTextLocal(tp.instanceOpt) ~ "'" // debug for now, so that we can see where the TypeVars are.
        else {
          val constr = ctx.typerState.constraint
          val bounds =
            if (constr.contains(tp)) constr.fullBounds(tp.origin)(ctx.addMode(Mode.Printing))
            else TypeBounds.empty
          if (ctx.settings.YshowVarBounds.value) "(" ~ toText(tp.origin) ~ "?" ~ toText(bounds) ~ ")"
          else toText(tp.origin)
        }
      case tp: LazyRef =>
        "LazyRef(" ~ toTextGlobal(tp.ref) ~ ")"
      case _ =>
        tp.fallbackToText(this)
    }
  }.close

  protected def polyParamName(name: TypeName): TypeName = name

  /** The name of the symbol without a unique id. Under refined printing,
   *  the decoded original name.
   */
  protected def simpleNameString(sym: Symbol): String = nameString(sym.name)

  /** If -uniqid is set, the hashcode of the polytype, after a # */
  protected def polyHash(pt: PolyType): Text =
    "#" + pt.hashCode provided ctx.settings.uniqid.value

  /** If -uniqid is set, the unique id of symbol, after a # */
  protected def idString(sym: Symbol): String =
    if (ctx.settings.uniqid.value) "#" + sym.id else ""

  def nameString(sym: Symbol): String =
    simpleNameString(sym) + idString(sym) // + "<" + (if (sym.exists) sym.owner else "") + ">"

  def fullNameString(sym: Symbol): String =
    if (sym.isRoot || sym == NoSymbol || sym.owner.isEffectiveRoot)
      nameString(sym)
    else
      fullNameString(fullNameOwner(sym)) + "." + nameString(sym)

  protected def fullNameOwner(sym: Symbol): Symbol = sym.effectiveOwner.enclosingClass

  protected def objectPrefix = "object "
  protected def packagePrefix = "package "

  protected def trimPrefix(text: Text) =
    text.stripPrefix(objectPrefix).stripPrefix(packagePrefix)

  protected def selectionString(tp: NamedType) =
    if (tp.currentSymbol.exists) nameString(tp.symbol)
    else nameString(tp.name)

  /** The string representation of this type used as a prefix */
  protected def toTextRef(tp: SingletonType): Text = controlled {
    tp match {
      case tp: TermRef =>
        toTextPrefix(tp.prefix) ~ selectionString(tp)
      case tp: ThisType =>
        nameString(tp.cls) + ".this"
      case SuperType(thistpe: SingletonType, _) =>
        toTextRef(thistpe).map(_.replaceAll("""\bthis$""", "super"))
      case SuperType(thistpe, _) =>
        "Super(" ~ toTextGlobal(thistpe) ~ ")"
      case tp @ ConstantType(value) =>
        toText(value)
      case MethodParam(mt, idx) =>
        nameString(mt.paramNames(idx))
      case tp: RefinedThis =>
        s"${nameString(tp.binder.typeSymbol)}{...}.this"
      case tp: SkolemType =>
        if (homogenizedView) toText(tp.info)
        else "<unknown instance of type " ~ toTextGlobal(tp.info) ~ ">"
    }
  }

  /** The string representation of this type used as a prefix */
  protected def toTextPrefix(tp: Type): Text = controlled {
    homogenize(tp) match {
      case NoPrefix => ""
      case tp: SingletonType => toTextRef(tp) ~ "."
      case tp => trimPrefix(toTextLocal(tp)) ~ "#"
    }
  }

  protected def isOmittablePrefix(sym: Symbol): Boolean =
    defn.UnqualifiedOwnerTypes.exists(_.symbol == sym) || isEmptyPrefix(sym)

  protected def isEmptyPrefix(sym: Symbol): Boolean =
    sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName

  /** String representation of a definition's type following its name,
   *  if symbol is completed, "?" otherwise.
   */
  protected def toTextRHS(optType: Option[Type]): Text = optType match {
    case Some(tp) => toTextRHS(tp)
    case None => "?"
  }

  /** String representation of a definition's type following its name */
  protected def toTextRHS(tp: Type): Text = controlled {
    homogenize(tp) match {
      case tp @ TypeBounds(lo, hi) =>
        if (lo eq hi) {
          val eql =
            if (tp.variance == 1) " =+ "
            else if (tp.variance == -1) " =- "
            else " = "
          eql ~ toText(lo)
        }
        else
          (if (lo isRef defn.NothingClass) Text() else " >: " ~ toText(lo)) ~
            (if (hi isRef defn.AnyClass) Text() else " <: " ~ toText(hi))
      case tp @ ClassInfo(pre, cls, cparents, decls, selfInfo) =>
        val preText = toTextLocal(pre)
        val (tparams, otherDecls) = decls.toList partition treatAsTypeParam
        val tparamsText =
          if (tparams.isEmpty) Text() else ("[" ~ dclsText(tparams) ~ "]").close
        val selfText: Text = selfInfo match {
          case NoType => Text()
          case sym: Symbol if !sym.isCompleted => "this: ? =>"
          case _ => "this: " ~ atPrec(InfixPrec) { toText(tp.selfType) } ~ " =>"
        }
        val trueDecls = otherDecls.filterNot(treatAsTypeArg)
        val declsText =
          if (trueDecls.isEmpty || !ctx.settings.debug.value) Text()
          else dclsText(trueDecls)
        tparamsText ~ " extends " ~ toTextParents(tp.parents) ~ "{" ~ selfText ~ declsText ~
          "} at " ~ preText
      case tp =>
        ": " ~ toTextGlobal(tp)
    }
  }

  protected def toTextParents(parents: List[Type]): Text = Text(parents.map(toTextLocal), " with ")

  protected def treatAsTypeParam(sym: Symbol): Boolean = false
  protected def treatAsTypeArg(sym: Symbol): Boolean = false

  /** String representation of symbol's kind. */
  def kindString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (flags is PackageClass) "package class"
    else if (flags is PackageVal) "package"
    else if (sym.isPackageObject)
      if (sym.isClass) "package object class"
      else "package object"
    else if (sym.isAnonymousClass) "anonymous class"
    else if (flags is ModuleClass) "module class"
    else if (flags is ModuleVal) "module"
    else if (flags is ImplClass) "implementation class"
    else if (flags is Trait) "trait"
    else if (sym.isClass) "class"
    else if (sym.isType) "type"
    else if (sym.isGetter) "getter"
    else if (sym.isSetter) "setter"
    else if (flags is Lazy) "lazy value"
    else if (flags is Mutable) "variable"
    else if (sym.isClassConstructor && sym.isPrimaryConstructor) "primary constructor"
    else if (sym.isClassConstructor) "constructor"
    else if (sym.is(Method)) "method"
    else if (sym.isTerm) "value"
    else ""
  }

  /** String representation of symbol's definition key word */
  protected def keyString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (flags is JavaTrait) "interface"
    else if ((flags is Trait) && !(flags is ImplClass)) "trait"
    else if (sym.isClass) "class"
    else if (sym.isType) "type"
    else if (flags is Mutable) "var"
    else if (flags is Package) "package"
    else if (flags is Module) "object"
    else if (sym is Method) "def"
    else if (sym.isTerm && (!(flags is Param))) "val"
    else ""
  }

  /** String representation of symbol's flags */
  protected def toTextFlags(sym: Symbol): Text =
    Text(sym.flagsUNSAFE.flagStrings map stringToText, " ")

  /** String representation of symbol's variance or "" if not applicable */
  protected def varianceString(sym: Symbol): String = varianceString(sym.variance)

  protected def varianceString(v: Int): String = v match {
    case -1 => "-"
    case 1 => "+"
    case _ => ""
  }

  def annotsText(sym: Symbol): Text = Text(sym.annotations.map(toText))

  def dclText(sym: Symbol): Text = dclTextWithInfo(sym, sym.unforcedInfo)

  def dclText(d: SingleDenotation): Text = dclTextWithInfo(d.symbol, Some(d.info))

  private def dclTextWithInfo(sym: Symbol, info: Option[Type]): Text =
    (toTextFlags(sym) ~~ keyString(sym) ~~
      (varianceString(sym) ~ nameString(sym)) ~ toTextRHS(info)).close

  def toText(sym: Symbol): Text =
    (kindString(sym) ~~ {
      if (sym.isAnonymousClass) toText(sym.info.parents, " with ") ~ "{...}"
      else if (hasMeaninglessName(sym)) simpleNameString(sym.owner) + idString(sym)
      else nameString(sym)
    }).close

  def locationText(sym: Symbol): Text =
    if (!sym.exists) ""
    else {
    val owns = sym.effectiveOwner
    if (owns.isClass && !isEmptyPrefix(owns)) " in " ~ toText(owns) else Text()
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
    case StringTag => "\"" + escapedString(const.value.toString) + "\""
    case ClazzTag => "classOf[" ~ toText(const.typeValue.classSymbol) ~ "]"
    case CharTag => s"'${escapedChar(const.charValue)}'"
    case LongTag => const.longValue.toString + "L"
    case EnumTag => const.symbolValue.name.toString
    case _ => String.valueOf(const.value)
  }

  def toText(annot: Annotation): Text = s"@${annot.symbol.name}" // for now

  protected def escapedString(str: String): String = str flatMap escapedChar

  def dclsText(syms: List[Symbol], sep: String): Text = Text(syms map dclText, sep)

  def toText(sc: Scope): Text =
    ("Scope{" ~ dclsText(sc.toList) ~ "}").close

  def toText[T >: Untyped](tree: Tree[T]): Text = {
    tree match {
      case node: Positioned =>
        def toTextElem(elem: Any): Text = elem match {
          case elem: Showable => elem.toText(this)
          case elem: List[_] => "List(" ~ Text(elem map toTextElem, ",") ~ ")"
          case elem => elem.toString
        }
        val nodeName = node.productPrefix
        val elems =
          Text(node.productIterator.map(toTextElem).toList, ", ")
        val tpSuffix =
          if (ctx.settings.printtypes.value && tree.hasType)
            " | " ~ toText(tree.typeOpt)
          else
            Text()

        nodeName ~ "(" ~ elems ~ tpSuffix ~ ")" ~ node.pos.toString
      case _ =>
        tree.fallbackToText(this)
    }
  }.close // todo: override in refined printer

  private var maxSummarized = Int.MaxValue

  def summarized[T](depth: Int)(op: => T): T = {
    val saved = maxSummarized
    maxSummarized = ctx.toTextRecursions + depth
    try op
    finally maxSummarized = depth
  }

  def summarized[T](op: => T): T = summarized(summarizeDepth)(op)

  def plain = this
}

