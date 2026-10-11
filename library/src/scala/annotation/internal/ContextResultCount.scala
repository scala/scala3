package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation that's automatically added for methods
 *  that have one or more nested context closures as their right hand side.
 *  The parameter `n` is an Int Literal that tells how many nested closures
 *  there are.
 *
 *  @param n the number of nested context closures in the method's right-hand side
 */
class ContextResultCount(n: Int) extends StaticAnnotation
