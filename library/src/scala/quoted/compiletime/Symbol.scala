package scala.quoted.compiletime

trait Symbol private[compiletime] () {

  // TODO: Add methods

}
object Symbol {

  def quoted(using quotes: Quotes): Symbol.Module = quotes.reflectV2.Symbol
  given moduleConversion: (quotes: Quotes) => Conversion[Symbol.type, Symbol.Module] = _ => quotes.reflectV2.Symbol

  trait Module private[compiletime] () {

    // TODO: Add methods

  }

}
