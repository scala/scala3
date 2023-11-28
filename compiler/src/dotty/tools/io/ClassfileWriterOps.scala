package dotty.tools.io

import dotty.tools.io.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.report
import scala.language.unsafeNulls
import scala.annotation.constructorOnly


/** Experimental usage - writes bytes to JarArchives */
class ClassfileWriterOps(outputDir: JarArchive)(using @constructorOnly ictx: Context) {

  type InternalName = String

  // if non-null, classfiles are written to a jar instead of the output directory
  private val jarWriter: JarWriter | Null =
    val localCtx = ictx
    outputDir.underlyingSource.map { source =>
      if outputDir.isEmpty then
        new Jar(source.file).jarWriter()
      else inContext(localCtx) {
        // Writing to non-empty JAR might be an undefined behaviour, e.g. in case if other files where
        // created using `AbstractFile.bufferedOutputStream`instead of JarWriter
        report.warning(em"Tried to write to non-empty JAR: $source")
        null
      }
    }.getOrElse(
      inContext(localCtx) {
        report.warning(em"tried to create a file writer for $outputDir, but it had no underlying source.")
        null
      }
    )

  def writeTasty(className: InternalName, bytes: Array[Byte]): Unit =
    writeToJar(className, bytes, ".tasty")

  private def writeToJar(className: InternalName, bytes: Array[Byte], suffix: String): Unit = {
    if (jarWriter == null) return
    val path = className + suffix
    val out = jarWriter.newOutputStream(path)
    try out.write(bytes, 0, bytes.length)
    finally out.flush()
  }

  def close(): Unit = {
    if (jarWriter != null) jarWriter.close()
    outputDir.close()
  }
}
