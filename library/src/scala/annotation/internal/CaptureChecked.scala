package scala.annotation
package internal
import annotation.experimental

/** A marker annotation on a toplevel class that indicates
 *  that the class was checked under -Ycc
 */
@experimental class CaptureChecked extends StaticAnnotation

