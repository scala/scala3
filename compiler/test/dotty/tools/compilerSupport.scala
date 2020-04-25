package dotty.tools

import javax.tools._
import java.io._
import java.nio.file._
import java.net.URI
import scala.jdk.CollectionConverters._
import dotty.tools.dotc._
import core._
import core.Contexts._
import dotc.core.Comments.{ContextDoc, ContextDocstrings}

// TODO: refactor, copy-pasted from Run#compileFromStrings
private def sourceFile(source: String, isJava: Boolean): util.SourceFile = {
  val uuid = java.util.UUID.randomUUID().toString
  val ext = if (isJava) ".java" else ".scala"
  val virtualFile = new io.VirtualFile(s"compileFromString-$uuid.$ext")
  val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
  writer.write(source)
  writer.close()
  new util.SourceFile(virtualFile, scala.io.Codec.UTF8)
}

/** Initialize a compiler context with the given classpath and
 *  pass it to `op`.
 */
def inCompilerContext[T](classpath: String, scalaSources: String*)(op: Context ?=> T): T =
  val compiler = Compiler()
  val run = compiler.newRun(initCtx(classpath))
  run.compileUnits(scalaSources.toList.map(s =>
    CompilationUnit(sourceFile(s, isJava = false))(using run.runContext)))
  op(using run.runContext)

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
