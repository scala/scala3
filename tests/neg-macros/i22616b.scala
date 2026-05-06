// This test illustrates a current limitation of quoted pattern type variables,
// which has been discussed in https://github.com/scala/scala3/issues/22616#issuecomment-3012534064:
// These type variables do not have bound in general (see `typedQuotedTypeVar`),
// so they might not conform to the expected type. Here, `t` does not conform
// to `String`.

import scala.quoted.{FromExpr, Expr, Quotes}

case class Foo(x: String)

object Macro:
  inline def myMacro(): Unit =
    ${ myMacroImpl('{Foo("hello")}) }

  def myMacroImpl(x: Expr[Foo])(using Quotes): Expr[Unit] =
    x match
      case '{ Foo($y: t) } => // error
        '{type S = t; ()} // error
      case _ =>
        println("not a foo")

    '{()}
