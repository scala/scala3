package repro

import scala.deriving.Mirror
import scala.quoted.*

object Macros {
  /** Summons `Mirror.Of` for each case of enum `T`, using the type of the case symbol. */
  inline def summonCaseMirrors[T]: Unit = ${ summonCaseMirrorsImpl[T] }
  def summonCaseMirrorsImpl[T: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    TypeRepr.of[T].typeSymbol.children.foreach(_.typeRef.asType match {
      case '[t] => Expr.summon[Mirror.Of[t]]
    })
    '{ () }
  }
}
