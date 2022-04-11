package dotty.tools

import scala.language.unsafeNulls

import javax.tools._
import java.io._
import java.nio.file._
import java.net.URI
import scala.jdk.CollectionConverters._
import dotty.tools.dotc._
import core._
import core.Contexts._
import dotc.core.Comments.{ContextDoc, ContextDocstrings}

/** Initialize a compiler context with the given `classpath`, compile all
 *  `scalaSources`, then run `op`.
 *
 *  If `separateRun` is true , then `op` will be run in a new run different from the
 *  one used to compile the sources, this makes it easier to test for potential
 *  issues involving retrieving symbols defined in a previous run.
 */
def inCompilerContext[T](classpath: String, separateRun: Boolean = true, scalaSources: String*)(op: Context ?=> T): T =
  val compiler = new Compiler()
  val rootCtx = initCtx(classpath)
  val firstRun = compiler.newRun(using rootCtx)
  firstRun.compileFromStrings(scalaSources.toList)
  val opRun = if separateRun
    then compiler.newRun(using rootCtx)
    else firstRun
  op(using opRun.runContext)

private def initCtx(classpath: String): Context =
  val ctx0 = (new ContextBase).initialCtx.fresh
  ctx0.setSetting(ctx0.settings.classpath, classpath)
  ctx0.setProperty(ContextDoc, new ContextDocstrings)
  ctx0

/** Compile `javaSources` with javac, then pass the compilation output directory
 *  to `op`. This directory will be deleted when op returns.
 */
def withJavaCompiled[T](javaSources: JavaFileObject*)(op: Path => T): T =
  val javaOutputDir = Files.createTempDirectory("withJavaCompiled")
  try
    val javac = ToolProvider.getSystemJavaCompiler()
    val options = List("-d", javaOutputDir.toString)
    javac.getTask(null, null, null, options.asJava, null, javaSources.asJava).call();
    op(javaOutputDir)
  finally
    deleteDirectory(javaOutputDir.toFile)

/** Recursively delete a directory. */
def deleteDirectory(directory: File): Unit =
  directory.listFiles.toList.foreach { file =>
    if (file.isDirectory)
      deleteDirectory(file)
    file.delete()
  }

class VirtualJavaSource(fileName: String, code: String) extends SimpleJavaFileObject(
    URI.create("string:///" + fileName), JavaFileObject.Kind.SOURCE):
  override def getCharContent(ignoreEncodingErrors: Boolean): String = code
