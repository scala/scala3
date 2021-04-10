package scala.annotation
package internal

/** An annotation that's automatically added for methods
 *  that have one or more nested context closures as their right hand side.
 *  The parameter `n` is an Int Literal that tells how many nested closures
 *  there are.
 */
class ContextResultCount(n: Int) extends StaticAnnotation
