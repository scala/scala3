package dotty.tools.scaladoc
package tasty

import scala.quoted._

/** Shorthand for `quotes.reflect` */
transparent inline def reflect(using q: Quotes): q.reflect.type = q.reflect
