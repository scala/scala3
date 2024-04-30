import scala.quoted.*

type P[+T] = ParsingRun[T]
trait ParsingRun[+T] {
  var successValue: Any
  def freshSuccessUnit(): ParsingRun[Unit]

}

object MacroInlineImpls {
  inline def flatMapXInline[T, V](
      lhs: ParsingRun[T]
  )(inline f: T => ParsingRun[V]): ParsingRun[V] = {
    f(lhs.successValue.asInstanceOf[T])
  }

  def parsedSequence0[T: Type, V: Type, R: Type](
      lhs: Expr[ParsingRun[T]],
      rhs: Expr[ParsingRun[V]]
  )(using quotes: Quotes): Expr[ParsingRun[R]] = {
    import quotes.reflect.*
    '{ $rhs.asInstanceOf[ParsingRun[R]] }
  }
}
