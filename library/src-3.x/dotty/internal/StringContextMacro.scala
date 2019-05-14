package dotty.internal

import scala.quoted._

object StringContextMacro {

  /** Implemetation of scala.StringContext.f used in Dotty while the standard library is still not bootstrapped */
  inline def f(sc: => StringContext)(args: Any*): String = ${ fImpl('sc, 'args) }

  private def fImpl(sc: Expr[StringContext], args: Expr[Seq[Any]]): Expr[String] = {
    // TODO implement f interpolation checks and generate optimal code
    //   See https://github.com/alemannosara/f-interpolators-in-Dotty-macros
    '{
      // Purely runtime implementation of the f interpolation without any checks
      val parts = $sc.parts.toList
      assert(parts.nonEmpty, "StringContext should have non empty parts")
      val parts2 = parts.head :: parts.tail.map(x => if (x.startsWith("%s")) x else "%s" + x)
      parts2.mkString.format($args: _*)
    }
  }

}
