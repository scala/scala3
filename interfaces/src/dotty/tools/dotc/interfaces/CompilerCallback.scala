package dotty.tools.dotc.interfaces
import scala.annotation.binaryCompatible


/** Set of callbacks called in response to events during the compilation process.
 *
 * You should implement this interface if you want to react to one or more of
 * these events.
 *
 * See the method `process` of `dotty.tools.dotc.Driver` for more information.
 */
@binaryCompatible
trait CompilerCallback {
  /** Called when a class has been generated.
   * @param source         The source file corresponding to this class.
   *                       Example: ./src/library/scala/collection/Seq.scala
   * @param generatedClass The generated classfile for this class.
   *                       Example: ./scala/collection/Seq$.class
   * @param className      The name of this class.
   *                       Example: scala.collection.Seq$
   */
  def onClassGenerated(source: SourceFile, generatedClass: AbstractFile, className: String): Unit

  /** Called when every class for this file has been generated.
   * @param source         The source file.
   *                       Example: ./src/library/scala/collection/Seq.scala
   */
  def onSourceCompiled(source: SourceFile): Unit 
}
