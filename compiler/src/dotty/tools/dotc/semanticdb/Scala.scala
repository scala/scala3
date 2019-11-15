package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.core.Symbols.{ Symbol => DottySymbol, defn }
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._

import scala.annotation.internal.sharable
import scala.annotation.switch

object Scala with

  object Symbols with

    val NoSymbol: String = ""
    val RootPackage: String = "_root_/"
    val EmptyPackage: String = "_empty_/"
    val LocalPrefix: String = "local"
    val PackageObjectDescriptor: String = "package."
    val s"${RootPackageName @ _}/" = RootPackage
    val s"${EmptyPackageName @ _}/" = EmptyPackage

    def displaySymbol(symbol: DottySymbol)(given Context): String =
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

  @sharable
  private val locals = raw"local(\d+)".r

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
