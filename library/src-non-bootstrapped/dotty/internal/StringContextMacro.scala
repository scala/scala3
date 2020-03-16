// ALWAYS KEEP THIS FILE IN src-non-bootstrapped

package dotty.internal

import scala.quoted._

object StringContextMacro {

  /** Implementation of scala.StringContext.f used in Dotty */
  inline def f(inline sc: StringContext)(args: Any*): String =
    scala.compiletime.error("Cannot expand f interpolator while bootstrapping the compiler")

}
