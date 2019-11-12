package dotty.tools.dotc.semanticdb

object Scala with

  object Symbols with

    val NoSymbol: String = ""
    val RootPackage: String = "_root_/"
    val EmptyPackage: String = "_empty_/"
    val LocalPrefix: String = "local"
    val PackageObjectDescriptor: String = "package."

  given (symbol: String) with

    def isNoSymbol: Boolean =
      symbol == Symbols.NoSymbol
    def isRootPackage: Boolean =
      symbol == Symbols.RootPackage
    def isEmptyPackage: Boolean =
      symbol == Symbols.EmptyPackage
    def isGlobal: Boolean =
      !symbol.isNoSymbol && !symbol.isMulti && (symbol.last match {
        case '.' | '#' | '/' | ')' | ']' => true
        case _                           => false
      })
    def isLocal: Boolean =
      symbol.startsWith(Symbols.LocalPrefix)
    def isMulti: Boolean =
      symbol.startsWith(";")
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
