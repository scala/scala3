package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.core
import core.Symbols.{ Symbol , defn, NoSymbol }
import core.Contexts._
import core.Names
import core.Names.Name
import core.Types.{Type, TypeBounds}
import core.Flags._
import core.NameKinds
import core.StdNames.nme
import SymbolInformation.{Kind => k}
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.core.Names.Designator

import java.lang.Character.{isJavaIdentifierPart, isJavaIdentifierStart}

import scala.annotation.internal.sharable
import scala.annotation.switch

object Scala3:
  import Symbols._
  import core.NameOps._

  @sharable private val unicodeEscape = raw"\$$u(\p{XDigit}{4})".r
  @sharable private val locals        = raw"local(\d+)".r
  @sharable private val ctor          = raw"[^;].*`<init>`\((?:\+\d+)?\)\.".r

  private val WILDCARDTypeName = nme.WILDCARD.toTypeName

  def range(span: Span, treeSource: SourceFile)(using Context): Option[Range] =
    def lineCol(offset: Int) = (treeSource.offsetToLine(offset), treeSource.column(offset))
    val (startLine, startCol) = lineCol(span.start)
    val (endLine, endCol) = lineCol(span.end)
    Some(Range(startLine, startCol, endLine, endCol))

  def namePresentInSource(desig: Designator, span: Span, source:SourceFile)(using Context): Boolean =
    if !span.exists then false
    else
      val content = source.content()
      val (start, end) =
        if content.lift(span.end - 1).exists(_ == '`') then
          (span.start + 1, span.end - 1)
        else (span.start, span.end)
      // println(s"${start}, $end")
      val nameInSource = content.slice(start, end).mkString
      // for secondary constructors `this`
      desig match
        case sym: Symbol =>
          if sym.isConstructor && nameInSource == nme.THISkw.toString then
            true
          else
            val target =
              if sym.isPackageObject then sym.owner
              else sym
            nameInSource == target.name.stripModuleClassSuffix.lastPart.toString
        case name: Name =>
          // println(nameInSource)
          // println(name.mangledString)
          nameInSource == name.mangledString

  sealed trait FakeSymbol {
    private[Scala3] var sname: Option[String] = None
  }

  /** Fake symbol that represents wildcard symbol which will be converted to
    * semanticdb symbol with
    * - name: local...
    * - SymbolInformation with signature TypeSignature of given type bound.
    */
  case class WildcardTypeSymbol(owner: Symbol, bounds: TypeBounds) extends FakeSymbol

  case class TermParamRefSymbol(owner: Symbol, name: Name, tp: Type) extends FakeSymbol
  case class TypeParamRefSymbol(owner: Symbol, name: Name, tp: TypeBounds) extends FakeSymbol
  case class RefinementSymbol(owner: Symbol, name: Name, tp: Type) extends FakeSymbol
  type SemanticSymbol = Symbol | FakeSymbol

  given SemanticSymbolOps : AnyRef with
    extension (sym: SemanticSymbol)
      def name(using Context): Name = sym match
        case s: Symbol => s.name
        case s: WildcardTypeSymbol => nme.WILDCARD
        case s: TermParamRefSymbol => s.name
        case s: TypeParamRefSymbol => s.name
        case s: RefinementSymbol => s.name

      def symbolName(using builder: SemanticSymbolBuilder)(using Context): String =
        sym match
          case s: Symbol => builder.symbolName(s)
          case s: FakeSymbol =>
            s.sname.getOrElse {
              val sname = builder.symbolName(s)
              s.sname = Some(sname)
              sname
            }

      def symbolInfo(symkinds: Set[SymbolKind])(using LinkMode, TypeOps, SemanticSymbolBuilder, Context): SymbolInformation =
        sym match
          case s: Symbol =>
            val kind = s.symbolKind(symkinds)
            val sname = sym.symbolName
            val signature = s.info.toSemanticSig(s)
            val symbolAnnotations = s.annotations.collect{
              case annot if annot.symbol != defn.BodyAnnot && annot.symbol != defn.ChildAnnot =>
                Annotation(annot.tree.tpe.toSemanticType(annot.symbol))
            }
            SymbolInformation(
              symbol = sname,
              language = Language.SCALA,
              kind = kind,
              properties = s.symbolProps(symkinds),
              displayName = Symbols.displaySymbol(s),
              signature = signature,
              access = s.symbolAccess(kind),
              overriddenSymbols = s.overriddenSymbols,
              annotations = symbolAnnotations
            )
          case s: WildcardTypeSymbol =>
            SymbolInformation(
              symbol = symbolName,
              language = Language.SCALA,
              kind = SymbolInformation.Kind.TYPE,
              displayName = nme.WILDCARD.show,
              signature = s.bounds.toSemanticSig(s.owner),
            )
          case s: TermParamRefSymbol =>
            SymbolInformation(
              symbol = symbolName,
              language = Language.SCALA,
              kind = SymbolInformation.Kind.PARAMETER,
              displayName = s.name.show.unescapeUnicode,
              signature = s.tp.toSemanticSig(s.owner),
            )
          case s: TypeParamRefSymbol =>
            SymbolInformation(
              symbol = symbolName,
              language = Language.SCALA,
              kind = SymbolInformation.Kind.TYPE_PARAMETER,
              displayName = s.name.show.unescapeUnicode,
              signature = s.tp.toSemanticSig(s.owner),
            )
          case s: RefinementSymbol =>
            val signature = s.tp.toSemanticSig(s.owner)
            val kind = signature match
              case _: TypeSignature => SymbolInformation.Kind.TYPE
              case _: MethodSignature => SymbolInformation.Kind.METHOD
              case _: ValueSignature => SymbolInformation.Kind.FIELD
              case _ => SymbolInformation.Kind.UNKNOWN_KIND
            SymbolInformation(
              symbol = symbolName,
              language = Language.SCALA,
              kind = kind,
              displayName = s.name.show.unescapeUnicode,
              properties =
                SymbolInformation.Property.ABSTRACT.value,
              signature = signature,
            )
  end SemanticSymbolOps

  enum SymbolKind derives CanEqual:
    kind =>

    case Val, Var, Setter, Abstract, TypeVal

    def isVar: Boolean = kind match
      case Var | Setter => true
      case _            => false
    def isVal: Boolean = kind == Val
    def isTypeVal: Boolean = kind == TypeVal
    def isVarOrVal: Boolean = kind.isVar || kind.isVal

  end SymbolKind

  object SymbolKind:
    val ValSet   = Set(Val)
    val VarSet   = Set(Var)
    val emptySet = Set.empty[SymbolKind]
  end SymbolKind

  object Symbols:

    val RootPackage: String = "_root_/"
    val EmptyPackage: String = "_empty_/"
    val LocalPrefix: String = "local"
    val PackageObjectDescriptor: String = "package."
    val s"${RootPackageName @ _}/" = RootPackage
    val s"${EmptyPackageName @ _}/" = EmptyPackage

    def displaySymbol(symbol: Symbol)(using Context): String =
      if symbol.isPackageObject then
        displaySymbol(symbol.owner)
      else if symbol.is(ModuleClass) then
        displaySymbol(symbol.sourceModule)
      else if symbol == defn.RootPackage then
        RootPackageName
      else if symbol.isEmptyPackage then
        EmptyPackageName
      else
        symbol.name.show

  end Symbols


  given NameOps: AnyRef with
    extension (name: Name)
      def isWildcard = name match
        case nme.WILDCARD | WILDCARDTypeName => true
        case _                               => name.is(NameKinds.WildcardParamName)

      def isScala2PackageObjectName: Boolean = name match
        case name: Names.TermName => name == nme.PACKAGE
        case name: Names.TypeName =>
          name.toTermName match
          case NameKinds.ModuleClassName(original) => original.isScala2PackageObjectName
          case _                                   => false

      def isEmptyNumbered: Boolean =
        !name.is(NameKinds.WildcardParamName)
        && !name.is(NameKinds.EvidenceParamName)
        && { name match
          case NameKinds.AnyNumberedName(nme.EMPTY, _) => true
          case _                                       => false
        }
  end NameOps

  given SymbolOps: AnyRef with
    extension (sym: Symbol)

      def ifExists(using Context): Option[Symbol] = if sym.exists then Some(sym) else None

      def isScala2PackageObject(using Context): Boolean =
        sym.name.isScala2PackageObjectName && sym.owner.is(Package) && sym.is(Module)

      def isAnonymous(using Context): Boolean =
        sym.isAnonymousClass
        || sym.isAnonymousModuleVal
        || sym.isAnonymousFunction

      def matchingSetter(using Context): Symbol =

        val setterName = sym.name.toTermName.setterName

        extension (t: Type) inline def matchingType = t.paramInfoss match
          case (arg::Nil)::Nil => t.resultType == defn.UnitType && arg == sym.info
          case _               => false

        sym.owner.info.decls.find(s => s.name == setterName && s.info.matchingType)

      /** Is symbol global? Non-global symbols get localN names */
      def isGlobal(using Context): Boolean =
        sym.exists && (
          sym.is(Package)
          || !sym.isSelfSym && (sym.is(Param) || sym.owner.isClass) && sym.owner.isGlobal
        )

      def isLocalWithinSameName(using Context): Boolean =
        sym.exists && !sym.isGlobal && sym.name == sym.owner.name

      /** Synthetic symbols that are not anonymous or numbered empty ident */
      def isSyntheticWithIdent(using Context): Boolean =
        sym.is(Synthetic) && !sym.isAnonymous && !sym.name.isEmptyNumbered

      /** The semanticdb name of the given symbol */
      def symbolName(using builder: SemanticSymbolBuilder)(using Context): String =
        builder.symbolName(sym)

      def funParamSymbol(using builder: SemanticSymbolBuilder)(using Context): Name => String =
        builder.funParamSymbol(sym)

      def symbolKind(symkinds: Set[SymbolKind])(using Context): SymbolInformation.Kind =
        if sym.isTypeParam then
          SymbolInformation.Kind.TYPE_PARAMETER
        else if sym.is(TermParam) then
          SymbolInformation.Kind.PARAMETER
        else if sym.isTerm && sym.owner.isTerm then
          SymbolInformation.Kind.LOCAL
        else if sym.isInlineMethod || sym.is(Macro) then
          SymbolInformation.Kind.MACRO
        else if sym.isConstructor then
          SymbolInformation.Kind.CONSTRUCTOR
        else if sym.isSelfSym then
          SymbolInformation.Kind.SELF_PARAMETER
        else if sym.isOneOf(Method) || symkinds.exists(_.isVarOrVal) then
          SymbolInformation.Kind.METHOD
        else if sym.isPackageObject then
          SymbolInformation.Kind.PACKAGE_OBJECT
        else if sym.is(Module) then
          SymbolInformation.Kind.OBJECT
        else if sym.is(Package) then
          SymbolInformation.Kind.PACKAGE
        else if sym.isAllOf(JavaInterface) then
          SymbolInformation.Kind.INTERFACE
        else if sym.is(Trait) then
          SymbolInformation.Kind.TRAIT
        else if sym.isClass then
          SymbolInformation.Kind.CLASS
        else if sym.isType then
          SymbolInformation.Kind.TYPE
        else if sym.is(ParamAccessor) then
          SymbolInformation.Kind.FIELD
        else
          SymbolInformation.Kind.UNKNOWN_KIND

      def symbolProps(symkinds: Set[SymbolKind])(using Context): Int =
        if sym.is(ModuleClass) then
          return sym.sourceModule.symbolProps(symkinds)
        var props = 0
        if sym.isPrimaryConstructor then
          props |= SymbolInformation.Property.PRIMARY.value
        if sym.is(Abstract) || symkinds.contains(SymbolKind.Abstract) then
          props |= SymbolInformation.Property.ABSTRACT.value
        if sym.is(Final) then
          props |= SymbolInformation.Property.FINAL.value
        if sym.is(Sealed) then
          props |= SymbolInformation.Property.SEALED.value
        if sym.isOneOf(GivenOrImplicit) then
          props |= SymbolInformation.Property.IMPLICIT.value
        if sym.is(Lazy, butNot=Module) then
          props |= SymbolInformation.Property.LAZY.value
        if sym.isAllOf(Case | Module) ||
          (sym.is(CaseClass) && !symkinds.exists(_.isTypeVal)) || // `t` of `case List[t] =>` (which has `CaseClass` flag) shouldn't be `CASE`
          sym.isAllOf(EnumCase) then
          props |= SymbolInformation.Property.CASE.value
        if sym.is(Covariant) then
          props |= SymbolInformation.Property.COVARIANT.value
        if sym.is(Contravariant) then
          props |= SymbolInformation.Property.CONTRAVARIANT.value
        if sym.isAllOf(DefaultMethod | JavaDefined) || sym.is(Accessor) && sym.name.is(NameKinds.DefaultGetterName) then
          props |= SymbolInformation.Property.DEFAULT.value
        if symkinds.exists(_.isVal) then
          props |= SymbolInformation.Property.VAL.value
        if symkinds.exists(_.isVar) then
          props |= SymbolInformation.Property.VAR.value
        if sym.is(JavaStatic) then
          props |= SymbolInformation.Property.STATIC.value
        if sym.is(Enum) then
          props |= SymbolInformation.Property.ENUM.value
        if sym.is(Given) then
          props |= SymbolInformation.Property.GIVEN.value
        if sym.is(Inline) then
          props |= SymbolInformation.Property.INLINE.value
        if sym.is(Open) then
          props |= SymbolInformation.Property.OPEN.value
        if sym.is(Open) then
          props |= SymbolInformation.Property.OPEN.value
        if sym.is(Transparent) then
          props |= SymbolInformation.Property.TRANSPARENT.value
        if sym.is(Infix) then
          props |= SymbolInformation.Property.INFIX.value
        if sym.is(Opaque) then
          props |= SymbolInformation.Property.OPAQUE.value
        props

      def symbolAccess(kind: SymbolInformation.Kind)(using Context, SemanticSymbolBuilder): Access =
        kind match
          case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER | k.PACKAGE | k.PACKAGE_OBJECT =>
            Access.Empty
          case _ =>
            if (sym.privateWithin == NoSymbol)
              if (sym.isAllOf(PrivateLocal)) PrivateThisAccess()
              else if (sym.is(Private)) PrivateAccess()
              else if (sym.isAllOf(ProtectedLocal)) ProtectedThisAccess()
              else if (sym.is(Protected)) ProtectedAccess()
              else PublicAccess()
            else
              val ssym = sym.privateWithin.symbolName
              if (sym.is(Protected)) ProtectedWithinAccess(ssym)
              else PrivateWithinAccess(ssym)

      def overriddenSymbols(using Context, SemanticSymbolBuilder): List[String] =
        sym.allOverriddenSymbols.map(_.symbolName).toList
  end SymbolOps

  object LocalSymbol:

    def unapply(symbolInfo: SymbolInformation): Option[Int] = symbolInfo.symbol match
      case locals(ints) =>
        val bi = BigInt(ints)
        if bi.isValidInt then
          Some(bi.toInt)
        else
          None

      case _ => None

  end LocalSymbol

  extension (char: Char)
    private inline def isGlobalTerminal = (char: @switch) match
      case '/' | '.' | '#' | ']' | ')' => true
      case _                           => false

  given StringOps: AnyRef with
    extension (symbol: String)
      def isSymbol: Boolean = !symbol.isEmpty
      def isRootPackage: Boolean = RootPackage == symbol
      def isEmptyPackage: Boolean = EmptyPackage == symbol

      def isGlobal: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last.isGlobalTerminal
      def isLocal: Boolean = !symbol.isEmpty && !symbol.isMulti && !symbol.last.isGlobalTerminal
      def isMulti: Boolean = symbol startsWith ";"

      def isConstructor: Boolean = ctor matches symbol
      def isPackage: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == '/'
      def isTerm: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == '.'
      def isType: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == '#'
      def isTypeParameter: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == ']'
      def isParameter: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == ')'

      def desc: Descriptor =
        if isGlobal then DescriptorParser(symbol)._1
        else Descriptor.None

      def unescapeUnicode =
        unicodeEscape.replaceAllIn(symbol, m => String.valueOf(Integer.parseInt(m.group(1), 16).toChar))

      def isJavaIdent =
        isJavaIdentifierStart(symbol.head) && symbol.tail.forall(isJavaIdentifierPart)
  end StringOps

  given InfoOps: AnyRef with
    extension (info: SymbolInformation)
      def isAbstract: Boolean = (info.properties & SymbolInformation.Property.ABSTRACT.value) != 0
      def isFinal: Boolean = (info.properties & SymbolInformation.Property.FINAL.value) != 0
      def isSealed: Boolean = (info.properties & SymbolInformation.Property.SEALED.value) != 0
      def isImplicit: Boolean = (info.properties & SymbolInformation.Property.IMPLICIT.value) != 0
      def isLazy: Boolean = (info.properties & SymbolInformation.Property.LAZY.value) != 0
      def isCase: Boolean = (info.properties & SymbolInformation.Property.CASE.value) != 0
      def isCovariant: Boolean = (info.properties & SymbolInformation.Property.COVARIANT.value) != 0
      def isContravariant: Boolean = (info.properties & SymbolInformation.Property.CONTRAVARIANT.value) != 0
      def isPrimary: Boolean = (info.properties & SymbolInformation.Property.PRIMARY.value) != 0
      def isVal: Boolean = (info.properties & SymbolInformation.Property.VAL.value) != 0
      def isVar: Boolean = (info.properties & SymbolInformation.Property.VAR.value) != 0
      def isStatic: Boolean = (info.properties & SymbolInformation.Property.STATIC.value) != 0
      def isEnum: Boolean = (info.properties & SymbolInformation.Property.ENUM.value) != 0
      def isDefault: Boolean = (info.properties & SymbolInformation.Property.DEFAULT.value) != 0
      def isGiven: Boolean = (info.properties & SymbolInformation.Property.GIVEN.value) != 0
      def isInline: Boolean = (info.properties & SymbolInformation.Property.INLINE.value) != 0
      def isOpen: Boolean = (info.properties & SymbolInformation.Property.OPEN.value) != 0
      def isTransparent: Boolean = (info.properties & SymbolInformation.Property.TRANSPARENT.value) != 0
      def isInfix: Boolean = (info.properties & SymbolInformation.Property.INFIX.value) != 0
      def isOpaque: Boolean = (info.properties & SymbolInformation.Property.OPAQUE.value) != 0

      def isUnknownKind: Boolean = info.kind.isUnknownKind
      def isLocal: Boolean = info.kind.isLocal
      def isField: Boolean = info.kind.isField
      def isMethod: Boolean = info.kind.isMethod
      def isConstructor: Boolean = info.kind.isConstructor
      def isMacro: Boolean = info.kind.isMacro
      def isType: Boolean = info.kind.isType
      def isParameter: Boolean = info.kind.isParameter
      def isSelfParameter: Boolean = info.kind.isSelfParameter
      def isTypeParameter: Boolean = info.kind.isTypeParameter
      def isObject: Boolean = info.kind.isObject
      def isPackage: Boolean = info.kind.isPackage
      def isPackageObject: Boolean = info.kind.isPackageObject
      def isClass: Boolean = info.kind.isClass
      def isTrait: Boolean = info.kind.isTrait
      def isInterface: Boolean = info.kind.isInterface
  end InfoOps

  given RangeOps: AnyRef with
    extension (range: Range)
      def hasLength = range.endLine > range.startLine || range.endCharacter > range.startCharacter
  end RangeOps

  private def compareRange(x: Option[Range], y: Option[Range]): Int = x -> y match
    case None -> _ | _ -> None => 0
    case Some(a) -> Some(b) =>
      val byLine = Integer.compare(a.startLine, b.startLine)
      if (byLine != 0)
        byLine
      else // byCharacter
        Integer.compare(a.startCharacter, b.startCharacter)

  /** Sort symbol occurrences by their start position. */
  given Ordering[SymbolOccurrence] = (x, y) => compareRange(x.range, y.range)

  given Ordering[SymbolInformation] = Ordering.by[SymbolInformation, String](_.symbol)(IdentifierOrdering())

  given Ordering[Synthetic] = (x, y) => compareRange(x.range, y.range)

  /**
    * A comparator for identifier like "Predef" or "Function10".
    *
    * Differences from the default string comparator:
    * - works with CharSequences like compiler `Name`
    * - orders numbers by their numerical value instead of lexicographical
    *   - Good: `Function1`, `Function2`,  `Function10`
    *   - Bad:  `Function1`, `Function10`, `Function2`
    *
    * taken from https://github.com/scalameta/scalameta/blob/master/semanticdb/metap/src/main/scala/scala/meta/internal/metap/IdentifierOrdering.scala
    */
  private class IdentifierOrdering[T <: CharSequence] extends Ordering[T]:

    override def compare(o1: T, o2: T): Int =
      val len = math.min(o1.length(), o2.length())
      var i = 0
      while i < len do
        val a = o1.charAt(i)
        val b = o2.charAt(i)
        if a.isDigit && b.isDigit then
          val byDigit = Integer.compare(toDigit(o1, i), toDigit(o2, i))
          if (byDigit != 0) return byDigit
          else
            i = seekNonDigit(o1, i)
        else
          val result = Character.compare(a, b)
          if result != 0 then
            return result
          i += 1
      end while
      Integer.compare(o1.length(), o2.length())
    end compare

    private def seekNonDigit(cs: T, i: Int): Int =
      var curr = i
      while curr < cs.length && cs.charAt(curr).isDigit do
        curr += 1
      curr
    end seekNonDigit

    private def toDigit(cs: T, i: Int): Int =
      val digit = cs.subSequence(i, seekNonDigit(cs, i))
      Integer.parseUnsignedInt(digit.toString)
    end toDigit

  end IdentifierOrdering

end Scala3
