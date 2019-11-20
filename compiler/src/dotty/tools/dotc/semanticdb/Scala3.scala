package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.core
import core.Symbols.{ Symbol , defn }
import core.Contexts.Context
import core.Names
import core.Names.Name
import core.Types.Type
import core.Flags._
import core.NameKinds
import core.StdNames.nme

import java.lang.Character.{isJavaIdentifierPart, isJavaIdentifierStart}

import scala.annotation.internal.sharable
import scala.annotation.switch

object Scala3 with
  import Symbols._
  import core.NameOps.given

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

  end SymbolKind

  object SymbolKind with
    val ValSet   = Set(Val)
    val VarSet   = Set(Var)
    val emptySet = Set.empty[SymbolKind]
  end SymbolKind

  object Symbols with

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

  end Symbols

  given NameOps: (name: Name) with

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
      && { name match
        case NameKinds.AnyNumberedName(nme.EMPTY, _) => true
        case _                                       => false
      }

  end NameOps

  given SymbolOps: (sym: Symbol) with

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

    /** Is symbol global? Non-global symbols get localN names */
    def isGlobal(given Context): Boolean =
      sym.is(Package)
      || !sym.isSelfSym && (sym.is(Param) || sym.owner.isClass) && sym.owner.isGlobal

    def isLocalWithinSameName(given Context): Boolean =
      sym.exists && !sym.isGlobal && sym.name == sym.owner.name

    /** Synthetic symbols that are not anonymous or numbered empty ident */
    def isSyntheticWithIdent(given Context): Boolean =
      sym.is(Synthetic) && !sym.isAnonymous && !sym.name.isEmptyNumbered

  end SymbolOps

  object LocalSymbol with

    def unapply(symbolInfo: SymbolInformation): Option[Int] = symbolInfo.symbol match
      case locals(ints) =>
        val bi = BigInt(ints)
        if bi.isValidInt
          Some(bi.toInt)
        else
          None

      case _ => None

  end LocalSymbol

  private inline def (char: Char) `is/.#])` = (char: @switch) match
    case '/' | '.' | '#' | ']' | ')' => true
    case _                           => false

  given StringOps: (symbol: String) with

    def isSymbol: Boolean = !symbol.isEmpty
    def isRootPackage: Boolean = RootPackage == symbol
    def isEmptyPackage: Boolean = EmptyPackage == symbol

    def isGlobal: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last.`is/.#])`
    def isLocal: Boolean = !symbol.isEmpty && !symbol.isMulti && !symbol.last.`is/.#])`
    def isMulti: Boolean = symbol startsWith ";"

    def isConstructor: Boolean = ctor matches symbol
    def isPackage: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == '/'
    def isTerm: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == '.'
    def isType: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == '#'
    def isTypeParameter: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == ']'
    def isParameter: Boolean = !symbol.isEmpty && !symbol.isMulti && symbol.last == ')'

    def unescapeUnicode =
      unicodeEscape.replaceAllIn(symbol, m => String.valueOf(Integer.parseInt(m.group(1), 16).toChar))

    def isJavaIdent =
      isJavaIdentifierStart(symbol.head) && symbol.tail.forall(isJavaIdentifierPart)

  end StringOps

  given InfoOps: (info: SymbolInformation) with

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

  given RangeOps: (range: Range) with
    def hasLength = range.endLine > range.startLine || range.endCharacter > range.startCharacter
  end RangeOps

end Scala3
