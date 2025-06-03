package crash.test

import scala.language.dynamics

import scala.quoted.*

object Export extends Dynamic:
  inline def applyDynamic(name: "apply")(inline args: Any*): Stack = ${
    applyDynamicImpl('args)
  }

  def applyDynamicImpl(args: Expr[Seq[Any]])(using Quotes): Expr[Stack] =
    import quotes.reflect.*

    '{ Stack("", Vector.empty) }
