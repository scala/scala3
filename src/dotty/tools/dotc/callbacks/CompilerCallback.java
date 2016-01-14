package dotty.tools.dotc.callbacks;

import java.io.File;

/** This interface contains methods that can be implemented to execute code during the
 *  compilation process.
 *
 *  NOTE: This interface is used for integration of Dotty compiler with Intellij IDEA.
 *  If you want to change it please contact Scala Plugin team.
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
public interface CompilerCallback {

    /** Called when a class has been generated.
     *
     *  @param source         The source file corresponding to this class.
     *                        Example: ./src/library/scala/collection/Seq.scala
     *  @param generatedClass The generated classfile for this class.
     *                        Example: ./scala/collection/Seq$.class
     *  @param className      The name of this class.
     *                        Example: scala.collection.Seq$
     */
    void onClassGenerated(File source, File generatedClass, String className);

    /** Called when every class for this file has been generated.
     *
     *  @param source         The source file.
     *                        Example: ./src/library/scala/collection/Seq.scala
     */
    void onSourceCompiled(File source);

    /** Called when compilation phase starts on a given source
     *
     *  @param phase          Name (or other identifier) of the current phase
     *  @param sourcePath     Path to the processed file
     */
    void startUnit(String phase, String sourcePath);

    /** Called to show progress of the compilation process
     */
    void advance(int currentProgress, int totalProgress);

    //Methods to pass compiler and debug messages and exceptions

    void info(String msg, Position pos);

    void warning(String msg, Position pos);

    void error(String msg, Position pos);

    void debug(String msg);

    void trace(Throwable exception);
}
