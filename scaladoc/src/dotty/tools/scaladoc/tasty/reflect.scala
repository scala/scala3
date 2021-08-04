package dotty.tools.scaladoc
package tasty

import scala.quoted._

/** Shorthand for `quotes.reflect` */
transparent inline def reflect(using inline q: Quotes): q.reflect.type = q.reflect
