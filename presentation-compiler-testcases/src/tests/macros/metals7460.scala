package tests.macros

import scala.quoted.*

object Macros7460 {

  transparent inline def foo: String =
    ${ fooImpl }

  private def fooImpl(using Quotes): Expr[String] =
    Expr("foo...")

  transparent inline def bar: String =
    ${ barImpl }

  private def barImpl(using Quotes): Expr[String] =
    quotes.reflect.Position.ofMacroExpansion.sourceFile.getJPath.get // this line is the culprit
    Expr("bar...")

}
