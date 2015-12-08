package dotty.tools.dotc

import java.io.File

/** This trait contains methods that can be overriden to execute code during the
 *  compilation process.
 *
 *  NOTE: This trait is experimental and may be subject to arbitrary changes.
 *
 *  Example usage:
 *  {{{
 *  val args: Array[String] = ...
 *  val callback = new CompilerCallback {
 *    override def onClassGenerated(source: File, generatedClass: File, className: String) =
 *      println(s"onClassGenerated($source, $generatedClass, $className)")
 *    override def onSourceCompiled(source: File) =
 *      println(s"onSourceCompiled($source)")
 *  }
 *  dotty.tools.dotc.process(args, callback)
 *  // Or, if you have a custom root context `rootCtx`:
 *  dotty.tools.dotc.process(args, rootCtx.setCompilerCallback(callback))
 *  }}}
 */
trait CompilerCallback {
  /** Called when a class has been generated.
   *
   *  @param source         The source file corresponding to this class.
   *                        Example: ./src/library/scala/collection/Seq.scala
   *  @param generatedClass The generated classfile for this class.
   *                        Example: ./scala/collection/Seq$.class
   *  @param className      The name of this class.
   *                        Example: scala.collection.Seq$
   */
  def onClassGenerated(source: File, generatedClass: File, className: String): Unit = {}

  /** Called when every class for this file has been generated.
   *
   *  @param source         The source file.
   *                        Example: ./src/library/scala/collection/Seq.scala
   */
  def onSourceCompiled(source: File): Unit = {}
}
