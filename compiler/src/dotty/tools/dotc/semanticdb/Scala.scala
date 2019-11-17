package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.core.Symbols.{ Symbol, defn }
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.NameOps
import dotty.tools.dotc.core.StdNames.nme

import scala.annotation.internal.sharable
import scala.annotation.switch

object Scala with
  import NameOps.given

  @sharable private val unicodeEscape = raw"\$$u(\p{XDigit}{4})".r
  @sharable private val locals        = raw"local(\d+)".r
  @sharable private val ctor          = raw"[^;].*`<init>`\((?:\+\d+)?\)\.".r

  private val WILDCARDTypeName = nme.WILDCARD.toTypeName

  enum SymbolKind derives Eql with
    kind =>

    case Val, Var, Setter, Abstract

    def isVar: Boolean = kind match
      case Var | Setter => true
      case _            => false

    def isVal: Boolean = kind == Val

    def isVarOrVal: Boolean = kind.isVar || kind.isVal

  object SymbolKind with
    val ValSet   = Set(Val)
    val VarSet   = Set(Var)
    val emptySet = Set.empty[SymbolKind]

  object Symbols with

    val NoSymbol: String = ""
    val RootPackage: String = "_root_/"
    val EmptyPackage: String = "_empty_/"
    val LocalPrefix: String = "local"
    val PackageObjectDescriptor: String = "package."
    val s"${RootPackageName @ _}/" = RootPackage
    val s"${EmptyPackageName @ _}/" = EmptyPackage

    def displaySymbol(symbol: Symbol)(given Context): String =
      if symbol.isPackageObject then
        displaySymbol(symbol.owner)
      else if symbol.is(ModuleClass) then
        displaySymbol(symbol.sourceModule)
      else if symbol == defn.RootPackage
        RootPackageName
      else if symbol.isEmptyPackage
        EmptyPackageName
      else
        symbol.name.show

    given symbolOps: (symbol: String) with
      def unescapeUnicode =
        unicodeEscape.replaceAllIn(symbol, m => String.valueOf(Integer.parseInt(m.group(1), 16).toChar))

  given nameOps: (name: Name) with

    def isWildcard = name match
      case nme.WILDCARD | WILDCARDTypeName => true
      case _                               => name.is(NameKinds.WildcardParamName)

    def isScala2PackageObjectName: Boolean = name match
      case name: Names.TermName => name == nme.PACKAGE
      case name: Names.TypeName =>
        name.toTermName match
        case NameKinds.ModuleClassName(original) => original.isScala2PackageObjectName
        case _                                   => false


  given symbolOps: (sym: Symbol) with

    def isScala2PackageObject(given Context): Boolean =
      sym.name.isScala2PackageObjectName && sym.owner.is(Package) && sym.is(Module)

    def isAnonymous(given Context): Boolean =
      sym.isAnonymousClass
      || sym.isAnonymousModuleVal
      || sym.isAnonymousFunction

    def matchingSetter(given Context): Symbol =

      val setterName = sym.name.toTermName.setterName

      inline def (t: Type) matchingType = t.paramInfoss match
        case (arg::Nil)::Nil => t.resultType == defn.UnitType && arg == sym.info
        case _               => false

      sym.owner.info.decls.find(s => s.name == setterName && s.info.matchingType)

  object LocalSymbol with
    def unapply(symbolInfo: SymbolInformation): Option[Int] =
      symbolInfo.symbol match
      case locals(ints) =>
        val bi = BigInt(ints)
        if bi.isValidInt then Some(bi.toInt) else None
      case _ => None

  given (symbol: String) with

    def isNoSymbol: Boolean =
      symbol == Symbols.NoSymbol
    def isRootPackage: Boolean =
      symbol == Symbols.RootPackage
    def isEmptyPackage: Boolean =
      symbol == Symbols.EmptyPackage
    def isGlobal: Boolean =
      !symbol.isNoSymbol
      && !symbol.isMulti
      && { (symbol.last: @switch) match
        case '.' | '#' | '/' | ')' | ']' => true
        case _                           => false
      }
    def isLocal: Boolean =
      locals matches symbol
    def isMulti: Boolean =
      symbol startsWith ";"
    def isConstructor: Boolean =
      ctor matches symbol
    def isTerm: Boolean =
      !symbol.isNoSymbol && !symbol.isMulti && symbol.last == '.'
    def isType: Boolean =
      !symbol.isNoSymbol && !symbol.isMulti && symbol.last == '#'
    def isPackage: Boolean =
      !symbol.isNoSymbol && !symbol.isMulti && symbol.last == '/'
    def isParameter: Boolean =
      !symbol.isNoSymbol && !symbol.isMulti && symbol.last == ')'
    def isTypeParameter: Boolean =
      !symbol.isNoSymbol && !symbol.isMulti && symbol.last == ']'

  given (info: SymbolInformation) with

    def isPrimary: Boolean =
      (info.properties & SymbolInformation.Property.PRIMARY.value) != 0
    def isAbstract: Boolean =
      (info.properties & SymbolInformation.Property.ABSTRACT.value) != 0
    def isFinal: Boolean =
      (info.properties & SymbolInformation.Property.FINAL.value) != 0
    def isSealed: Boolean =
      (info.properties & SymbolInformation.Property.SEALED.value) != 0
    def isImplicit: Boolean =
      (info.properties & SymbolInformation.Property.IMPLICIT.value) != 0
    def isLazy: Boolean =
      (info.properties & SymbolInformation.Property.LAZY.value) != 0
    def isCase: Boolean =
      (info.properties & SymbolInformation.Property.CASE.value) != 0
    def isCovariant: Boolean =
      (info.properties & SymbolInformation.Property.COVARIANT.value) != 0
    def isContravariant: Boolean =
      (info.properties & SymbolInformation.Property.CONTRAVARIANT.value) != 0
    def isVal: Boolean =
      (info.properties & SymbolInformation.Property.VAL.value) != 0
    def isVar: Boolean =
      (info.properties & SymbolInformation.Property.VAR.value) != 0
    def isStatic: Boolean =
      (info.properties & SymbolInformation.Property.STATIC.value) != 0
    def isEnum: Boolean =
      (info.properties & SymbolInformation.Property.ENUM.value) != 0
    def isDefault: Boolean =
      (info.properties & SymbolInformation.Property.DEFAULT.value) != 0
