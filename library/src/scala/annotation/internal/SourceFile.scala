package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation

/** An annotation to record the source file path of a definition.
 *
 *  @param path the path of the source file in which the annotated definition appears
 */
class SourceFile(path: String) extends Annotation {

}
