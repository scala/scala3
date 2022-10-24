package scala.annotation
package internal
import annotation.experimental

/** A marker annotation on a toplevel class that indicates
 *  that the class was typed with the pureFunctions language import.
 */
@experimental class WithPureFuns extends StaticAnnotation

