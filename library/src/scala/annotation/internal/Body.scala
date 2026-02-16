package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation

/** The class associated with a `BodyAnnotation`, which indicates
 *  an inline method's right hand side
 */
final class Body() extends Annotation
