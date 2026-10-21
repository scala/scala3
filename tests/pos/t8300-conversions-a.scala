// cf. pos/t8300-conversions-b.scala
trait Universe {
  type Symbol >: Null <: AnyRef & SymbolApi
  trait SymbolApi

  type TypeSymbol >: Null <: Symbol & TypeSymbolApi
  trait TypeSymbolApi extends SymbolApi

  type FreeTypeSymbol >: Null <: TypeSymbol & FreeTypeSymbolApi
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
