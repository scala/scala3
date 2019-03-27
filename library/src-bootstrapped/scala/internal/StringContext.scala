package scala.internal

import scala.quoted._

object StringContext {

  inline def f(sc: => scala.StringContext)(args: Any*): String = ${ fImpl('sc, 'args) }

  private def fImpl(sc: Expr[StringContext], args: Expr[Seq[Any]]): Expr[String] = {
    // TODO implement f interpolation checks and addapt sc.parts
    //   See https://github.com/alemannosara/f-interpolators-in-Dotty-macros
    '{ $sc.parts.mkString.format($args: _*) }
  }

}
