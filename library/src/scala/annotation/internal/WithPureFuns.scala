package scala.annotation
package internal

import language.experimental.captureChecking

/** A marker annotation on a toplevel class that indicates
 *  that the class was typed with the pureFunctions language import.
 */
class WithPureFuns extends StaticAnnotation

