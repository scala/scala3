trait Universe {
  type Symbol >: Null <: AnyRef & SymbolApi
  trait SymbolApi

  type TypeSymbol >: Null <: TypeSymbolApi & Symbol
  trait TypeSymbolApi

  implicit class CompatibleSymbol(sym: Symbol) {
    def asFreeType: TypeSymbol = ???
  }
}

object Test extends App {
  val u: Universe = ???
  import u.*

  val sym: Symbol = ???
  sym.asFreeType
}
