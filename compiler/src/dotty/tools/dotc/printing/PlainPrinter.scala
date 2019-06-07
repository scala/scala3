package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Constants._, Denotations._
import Contexts.Context, Scopes.Scope, Denotations.Denotation, Annotations.Annotation
import StdNames.nme
import ast.Trees._
import typer.Implicits._
import typer.ImportInfo
import util.SourcePosition
import java.lang.Integer.toOctalString
import config.Config.summarizeDepth
import scala.util.control.NonFatal
import scala.annotation.switch

class PlainPrinter(_ctx: Context) extends Printer {
  protected[this] implicit def ctx: Context = _ctx.addMode(Mode.Printing)
  protected[this] def printDebug = ctx.settings.YprintDebug.value

  private[this] var openRecs: List[RecType] = Nil

  protected def maxToTextRecursions: Int = 100

  protected final def controlled(op: => Text): Text =
    if (ctx.base.toTextRecursions < maxToTextRecursions && ctx.base.toTextRecursions < maxSummarized)
      try {
        ctx.base.toTextRecursions += 1
        op
      } finally {
        ctx.base.toTextRecursions -= 1
      }
    else {
      if (ctx.base.toTextRecursions >= maxToTextRecursions)
        recursionLimitExceeded()
      "..."
    }

  protected def recursionLimitExceeded(): Unit = {
    ctx.warning("Exceeded recursion depth attempting to print.")
    if (ctx.debug) Thread.dumpStack()
  }

  /** If true, tweak output so it is the same before and after pickling */
  protected def homogenizedView: Boolean = ctx.settings.YtestPickler.value
  protected def debugPos: Boolean = ctx.settings.YdebugPos.value

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
        case tp: SkolemType =>
          homogenize(tp.info)
        case tp: LazyRef =>
          homogenize(tp.ref)
        case AppliedType(tycon, args) =>
          tycon.dealias.appliedTo(args)
        case _ =>
          tp
      }
    else tp

  private def sameBound(lo: Type, hi: Type): Boolean =
    try lo frozen_=:= hi catch { case NonFatal(ex) => false }

  private def homogenizeArg(tp: Type) = tp match {
    case TypeBounds(lo, hi) if homogenizedView && sameBound(lo, hi) => homogenize(hi)
    case _ => tp
  }

  private def selfRecName(n: Int) = s"z$n"

  /** If the name of the symbol's owner should be used when you care about
   *  seeing an interesting name: in such cases this symbol is e.g. a method
   *  parameter with a synthetic name, a constructor named "this", an object
   *  "package", etc.  The kind string, if non-empty, will be phrased relative
   *  to the name of the owner.
   */
  protected def hasMeaninglessName(sym: Symbol): Boolean = (
       (sym is Param) && sym.owner.isSetter    // x$1
    || sym.isClassConstructor                  // this
    || (sym.name == nme.PACKAGE)               // package
  )

  def nameString(name: Name): String = name.toString

  def toText(name: Name): Text = Str(nameString(name))

  /** String representation of a name used in a refinement
   *  In refined printing this undoes type parameter expansion
   */
  protected def refinementNameString(tp: RefinedType): String = nameString(tp.refinedName)

  /** String representation of a refinement */
  protected def toTextRefinement(rt: RefinedType): Closed =
    (refinementNameString(rt) ~ toTextRHS(rt.refinedInfo)).close

  protected def argText(arg: Type): Text = homogenizeArg(arg) match {
    case arg: TypeBounds => "_" ~ toText(arg)
    case arg => toText(arg)
  }

  /** Pretty-print comma-separated type arguments for a constructor to be inserted among parentheses or brackets
    * (hence with `GlobalPrec` precedence).
    */
  protected def argsText(args: List[Type]): Text = atPrec(GlobalPrec) { Text(args.map(arg => argText(arg) ), ", ") }

  /** The longest sequence of refinement types, starting at given type
   *  and following parents.
   */
  private def refinementChain(tp: Type): List[Type] =
    tp :: (tp match {
      case tp: RefinedType => refinementChain(tp.parent.stripTypeVar)
      case _ => Nil
    })

  /** Direct references to these symbols are printed without their prefix for convenience.
   *  They are either aliased in scala.Predef or in the scala package object.
   */
  private[this] lazy val printWithoutPrefix: Set[Symbol] =
    (defn.ScalaPredefModuleRef.typeAliasMembers
      ++ defn.ScalaPackageObjectRef.typeAliasMembers).map(_.info.classSymbol).toSet

  def toText(tp: Type): Text = controlled {
    homogenize(tp) match {
      case tp: TypeType =>
        toTextRHS(tp)
      case tp: TermRef
      if !tp.denotationIsCurrent && !homogenizedView || // always print underlying when testing picklers
         tp.symbol.is(Module) || tp.symbol.name == nme.IMPORT =>
        toTextRef(tp) ~ ".type"
      case tp: TermRef if tp.denot.isOverloaded =>
        "<overloaded " ~ toTextRef(tp) ~ ">"
      case tp: TypeRef =>
        if (printWithoutPrefix.contains(tp.symbol))
          toText(tp.name)
        else
          toTextPrefix(tp.prefix) ~ selectionString(tp)
      case tp: TermParamRef =>
        ParamRefNameString(tp) ~ ".type"
      case tp: TypeParamRef =>
        ParamRefNameString(tp) ~ lambdaHash(tp.binder)
      case tp: SingletonType =>
        toTextSingleton(tp)
      case AppliedType(tycon, args) =>
        (toTextLocal(tycon) ~ "[" ~ argsText(args) ~ "]").close
      case tp: RefinedType =>
        val parent :: (refined: List[RefinedType @unchecked]) =
          refinementChain(tp).reverse
        toTextLocal(parent) ~ "{" ~ Text(refined map toTextRefinement, "; ").close ~ "}"
      case tp: RecType =>
        try {
          openRecs = tp :: openRecs
          "{" ~ selfRecName(openRecs.length) ~ " => " ~ toTextGlobal(tp.parent) ~ "}"
        }
        finally openRecs = openRecs.tail
      case AndType(tp1, tp2) =>
        changePrec(AndTypePrec) { toText(tp1) ~ " & " ~ atPrec(AndTypePrec + 1) { toText(tp2) } }
      case OrType(tp1, tp2) =>
        changePrec(OrTypePrec) { toText(tp1) ~ " | " ~ atPrec(OrTypePrec + 1) { toText(tp2) } }
      case MatchType(bound, scrutinee, cases) =>
        changePrec(GlobalPrec) {
          def caseText(tp: Type): Text = tp match {
            case defn.MatchCase(pat, body) => "case " ~ toText(pat) ~ " => " ~ toText(body)
            case _ => "case " ~ toText(tp)
          }
          def casesText = Text(cases.map(caseText), "\n")
            atPrec(InfixPrec) { toText(scrutinee) } ~
            keywordStr(" match ") ~ "{" ~ casesText ~ "}" ~
            (" <: " ~ toText(bound) provided !bound.isRef(defn.AnyClass))
        }.close
      case tp: ErrorType =>
        s"<error ${tp.msg.msg}>"
      case tp: WildcardType =>
        if (tp.optBounds.exists) "(?" ~ toTextRHS(tp.bounds) ~ ")" else "?"
      case NoType =>
        "<notype>"
      case NoPrefix =>
        "<noprefix>"
      case tp: MethodType =>
        changePrec(GlobalPrec) {
          (if (tp.isContextualMethod) " given" else "") ~
          (if (tp.isErasedMethod) " erased" else "") ~~
          ("(" + (if (tp.isImplicitMethod && !tp.isContextualMethod) "implicit " else "")) ~
          paramsText(tp) ~
          (if (tp.resultType.isInstanceOf[MethodType]) ")" else "): ") ~
          toText(tp.resultType)
        }
      case tp: ExprType =>
        changePrec(GlobalPrec) { "=> " ~ toText(tp.resultType) }
      case tp: TypeLambda =>
        changePrec(GlobalPrec) {
          "[" ~ paramsText(tp) ~ "]" ~ lambdaHash(tp) ~
          (Str(" => ") provided !tp.resultType.isInstanceOf[MethodType]) ~
          toTextGlobal(tp.resultType)
        }
      case AnnotatedType(tpe, annot) =>
        toTextLocal(tpe) ~ " " ~ toText(annot)
      case tp: TypeVar =>
        if (tp.isInstantiated)
          toTextLocal(tp.instanceOpt) ~ (Str("^") provided printDebug)
        else {
          val constr = ctx.typerState.constraint
          val bounds =
            if (constr.contains(tp)) ctx.addMode(Mode.Printing).typeComparer.fullBounds(tp.origin)
            else TypeBounds.empty
          if (bounds.isTypeAlias) toText(bounds.lo) ~ (Str("^") provided printDebug)
          else if (ctx.settings.YshowVarBounds.value) "(" ~ toText(tp.origin) ~ "?" ~ toText(bounds) ~ ")"
          else toText(tp.origin)
        }
      case tp: LazyRef =>
        def refTxt =
          try toTextGlobal(tp.ref)
          catch {
            case ex: Throwable => Str("...")
          }
        "LazyRef(" ~ refTxt ~ ")"
      case _ =>
        tp.fallbackToText(this)
    }
  }.close

  def toTextSingleton(tp: SingletonType): Text =
    toTextLocal(tp.underlying) ~ "(" ~ toTextRef(tp) ~ ")"

  protected def paramsText(tp: LambdaType): Text = {
    def paramText(name: Name, tp: Type) = toText(name) ~ toTextRHS(tp)
    Text((tp.paramNames, tp.paramInfos).zipped.map(paramText), ", ")
  }

  protected def ParamRefNameString(name: Name): String = name.toString

  protected def ParamRefNameString(param: ParamRef): String =
    ParamRefNameString(param.binder.paramNames(param.paramNum))

  /** The name of the symbol without a unique id. */
  protected def simpleNameString(sym: Symbol): String = nameString(sym.name)

  /** If -uniqid is set, the hashcode of the lambda type, after a # */
  protected def lambdaHash(pt: LambdaType): Text =
    if (ctx.settings.uniqid.value)
      try "#" + pt.hashCode
      catch { case ex: NullPointerException => "" }
    else ""

  /** If -uniqid is set, the unique id of symbol, after a # */
  protected def idString(sym: Symbol): String =
    if (ctx.settings.uniqid.value || Printer.debugPrintUnique) "#" + sym.id else ""

  def nameString(sym: Symbol): String =
    simpleNameString(sym) + idString(sym) // + "<" + (if (sym.exists) sym.owner else "") + ">"

  def fullNameString(sym: Symbol): String =
    if (sym.isRoot || sym == NoSymbol || sym.owner.isEffectiveRoot)
      nameString(sym)
    else
      fullNameString(fullNameOwner(sym)) + "." + nameString(sym)

  protected def fullNameOwner(sym: Symbol): Symbol = sym.effectiveOwner.enclosingClass

  protected def objectPrefix: String = "object "
  protected def packagePrefix: String = "package "

  protected def trimPrefix(text: Text): Text =
    text.stripPrefix(objectPrefix).stripPrefix(packagePrefix)

  protected def selectionString(tp: NamedType): String = {
    val sym = if (homogenizedView) tp.symbol else tp.currentSymbol
    if (sym.exists) nameString(sym) else nameString(tp.name)
  }

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
      case pref: TermParamRef =>
        nameString(pref.binder.paramNames(pref.paramNum))
      case tp: RecThis =>
        val idx = openRecs.reverse.indexOf(tp.binder)
        if (idx >= 0) selfRecName(idx + 1)
        else "{...}.this" // TODO move underlying type to an addendum, e.g. ... z3 ... where z3: ...
      case tp: SkolemType =>
        if (homogenizedView) toText(tp.info)
        else if (ctx.settings.XprintTypes.value) "<" ~ toText(tp.repr) ~ ":" ~ toText(tp.info) ~ ">"
        else toText(tp.repr)
    }
  }

  /** The string representation of this type used as a prefix, including separator */
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
      case tp: AliasingBounds =>
        " = " ~ toText(tp.alias)
      case tp @ TypeBounds(lo, hi) =>
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
          if (trueDecls.isEmpty || !ctx.settings.Ydebug.value) Text()
          else dclsText(trueDecls)
        tparamsText ~ " extends " ~ toTextParents(tp.parents) ~~ "{" ~ selfText ~ declsText ~
          "} at " ~ preText
      case mt: MethodType =>
        toTextGlobal(mt)
      case tp: ExprType =>
        ": => " ~ toTextGlobal(tp.widenExpr)
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

  /** String representation of symbol's definition keyword */
  protected def keyString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (flags is JavaTrait) "interface"
    else if (flags is Trait) "trait"
    else if (flags is Module) "object"
    else if (sym.isClass) "class"
    else if (sym.isType) "type"
    else if (flags is Mutable) "var"
    else if (flags is Package) "package"
    else if (sym is Method) "def"
    else if (sym.isTerm && (!(flags is Param))) "val"
    else ""
  }

  /** String representation of symbol's flags */
  protected def toTextFlags(sym: Symbol): Text =
    Text(sym.flagsUNSAFE.flagStrings(nameString(sym.privateWithin.name)) map stringToText, " ")

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
      if (sym.isAnonymousClass) toTextParents(sym.info.parents) ~~ "{...}"
      else if (hasMeaninglessName(sym) && !printDebug) simpleNameString(sym.owner) + idString(sym)
      else nameString(sym)
    }).close

  def locationText(sym: Symbol): Text =
    if (!sym.exists) ""
    else {
      val ownr = sym.effectiveOwner
      if (ownr.isClass && !isEmptyPrefix(ownr)) " in " ~ toText(ownr) else Text()
    }

  def locatedText(sym: Symbol): Text =
    (toText(sym) ~ locationText(sym)).close

  def extendedLocationText(sym: Symbol): Text =
    if (!sym.exists) ""
    else {
      def recur(ownr: Symbol, innerLocation: String): Text = {
        def nextOuter(innerKind: String): Text =
          recur(ownr.effectiveOwner,
            if (!innerLocation.isEmpty) innerLocation
            else s" in an anonymous $innerKind")
        def showLocation(ownr: Symbol, where: String): Text =
          innerLocation ~ " " ~ where ~ " " ~ toText(ownr)
        if (ownr.isAnonymousClass) nextOuter("class")
        else if (ownr.isAnonymousFunction) nextOuter("function")
        else if (isEmptyPrefix(ownr)) ""
        else if (ownr.isLocalDummy) showLocation(ownr.owner, "locally defined in")
        else if (ownr.isTerm && !ownr.is(Module | Method)) showLocation(ownr, "in the initializer of")
        else showLocation(ownr, "in")
      }
      recur(sym.owner, "")
    }

  def toText(denot: Denotation): Text = toText(denot.symbol) ~ "/D"

  private def escapedChar(ch: Char): String = (ch: @switch) match {
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
    case StringTag => stringText("\"" + escapedString(const.value.toString) + "\"")
    case ClazzTag => "classOf[" ~ toText(const.typeValue) ~ "]"
    case CharTag => literalText(s"'${escapedChar(const.charValue)}'")
    case LongTag => literalText(const.longValue.toString + "L")
    case EnumTag => literalText(const.symbolValue.name.toString)
    case ScalaSymbolTag => literalText("'" + const.scalaSymbolValue.name.toString)
    case _ => literalText(String.valueOf(const.value))
  }

  def toText(annot: Annotation): Text = s"@${annot.symbol.name}" // for now

  protected def escapedString(str: String): String = str flatMap escapedChar

  def dclsText(syms: List[Symbol], sep: String): Text = Text(syms map dclText, sep)

  def toText(sc: Scope): Text =
    ("Scope{" ~ dclsText(sc.toList) ~ "}").close

  def toText[T >: Untyped](tree: Tree[T]): Text = {
    def toTextElem(elem: Any): Text = elem match {
      case elem: Showable => elem.toText(this)
      case elem: List[_] => "List(" ~ Text(elem map toTextElem, ",") ~ ")"
      case elem => elem.toString
    }
    val nodeName = tree.productPrefix
    val elems =
      Text(tree.productIterator.map(toTextElem).toList, ", ")
    val tpSuffix =
      if (ctx.settings.XprintTypes.value && tree.hasType)
        " | " ~ toText(tree.typeOpt)
      else
        Text()

    nodeName ~ "(" ~ elems ~ tpSuffix ~ ")" ~ (Str(tree.sourcePos.toString) provided printDebug)
  }.close // todo: override in refined printer

  def toText(pos: SourcePosition): Text = {
    if (!pos.exists) "<no position>"
    else if (pos.source.exists) s"${pos.source.file.name}:${pos.line + 1}"
    else s"(no source file, offset = ${pos.span.point})"
  }

  def toText(result: SearchResult): Text = result match {
    case result: SearchSuccess =>
      "SearchSuccess: " ~ toText(result.ref) ~ " via " ~ toText(result.tree)
    case result: SearchFailure =>
      result.reason match {
        case _: NoMatchingImplicits => "No Matching Implicit"
        case _: DivergingImplicit => "Diverging Implicit"
        case result: AmbiguousImplicits =>
          "Ambiguous Implicit: " ~ toText(result.alt1.ref) ~ " and " ~ toText(result.alt2.ref)
        case _ =>
          "Search Failure: " ~ toText(result.tree)
    }
  }

  def toText(importInfo: ImportInfo): Text = {
    val siteStr = importInfo.site.show
    val exprStr = if (siteStr endsWith ".type") siteStr dropRight 5 else siteStr
    val selectorStr = importInfo.selectors match {
      case Ident(name) :: Nil => name.show
      case _ => "{...}"
    }
    s"import $exprStr.$selectorStr"
  }


  private[this] var maxSummarized = Int.MaxValue

  def summarized[T](depth: Int)(op: => T): T = {
    val saved = maxSummarized
    maxSummarized = ctx.base.toTextRecursions + depth
    try op
    finally maxSummarized = saved
  }

  def summarized[T](op: => T): T = summarized(summarizeDepth)(op)

  def plain: PlainPrinter = this

  protected def keywordStr(text: String): String = coloredStr(text, SyntaxHighlighting.KeywordColor)
  protected def keywordText(text: String): Text = coloredStr(text, SyntaxHighlighting.KeywordColor)
  protected def valDefText(text: Text): Text = coloredText(text, SyntaxHighlighting.ValDefColor)
  protected def typeText(text: Text): Text = coloredText(text, SyntaxHighlighting.TypeColor)
  protected def literalText(text: Text): Text = coloredText(text, SyntaxHighlighting.LiteralColor)
  protected def stringText(text: Text): Text = coloredText(text, SyntaxHighlighting.StringColor)

  protected def coloredStr(text: String, color: String): String =
    if (ctx.useColors) color + text + SyntaxHighlighting.NoColor else text
  protected def coloredText(text: Text, color: String): Text =
    if (ctx.useColors) color ~ text ~ SyntaxHighlighting.NoColor else text
}

