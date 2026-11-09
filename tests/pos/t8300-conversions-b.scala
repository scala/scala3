// cf. pos/t8300-conversions-a.scala
trait Universe {
  type Symbol >: Null <: AnyRef & SymbolApi
  trait SymbolApi

  type TypeSymbol >: Null <: TypeSymbolApi & Symbol
  trait TypeSymbolApi extends SymbolApi

  type FreeTypeSymbol >: Null <: FreeTypeSymbolApi & TypeSymbol
  trait FreeTypeSymbolApi extends TypeSymbolApi

  implicit class CompatibleSymbol(sym: Symbol) {
    def asFreeType: FreeTypeSymbol = ???
  }
}

object Test extends App {
  val u: Universe = ???
  import u.*

  val sym: Symbol = ???
  sym.asFreeType
}
