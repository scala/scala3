package scala

package object quoted {

  def run[T](expr: given QuoteContext => Expr[T]) given (toolbox: Toolbox): T =
    throw new Exception("Non bootsrapped library")

  def withQuoteContext[T](thunk: given QuoteContext => T) given (toolbox: Toolbox): T =
    throw new Exception("Non bootsrapped library")

}
