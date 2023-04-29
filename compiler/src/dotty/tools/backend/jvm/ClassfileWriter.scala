package dotty.tools.backend.jvm

import java.io.{DataOutputStream, IOException, PrintWriter, StringWriter}
import java.nio.file.Files
import java.util.jar.Attributes.Name

import scala.tools.asm.ClassReader
import scala.tools.asm.tree.ClassNode
import dotty.tools.io.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.util.NoSourcePosition
import java.nio.charset.StandardCharsets
import java.nio.channels.ClosedByInterruptException
import BTypes.InternalName
import scala.language.unsafeNulls

class ClassfileWriter(frontendAccess: PostProcessorFrontendAccess) {
  import frontendAccess.{backendReporting, compilerSettings}

  // if non-null, classfiles are additionally written to this directory
  private val dumpOutputDir: AbstractFile = getDirectoryOrNull(compilerSettings.dumpClassesDirectory)

  // if non-null, classfiles are written to a jar instead of the output directory
  private val jarWriter: JarWriter | Null = compilerSettings.outputDirectory match {
    case jar: JarArchive =>
      val mainClass = compilerSettings.mainClass.orElse {
        // If no main class was specified, see if there's only one
        // entry point among the classes going into the jar.
        frontendAccess.getEntryPoints match {
          case name :: Nil =>
            backendReporting.log(i"Unique entry point: setting Main-Class to $name")
            Some(name)
          case names =>
            if names.isEmpty then backendReporting.warning(em"No Main-Class designated or discovered.")
            else backendReporting.warning(em"No Main-Class due to multiple entry points:\n  ${names.mkString("\n  ")}")
            None
        }
      }
      jar.underlyingSource.map{ source =>
        if jar.isEmpty then
          val jarMainAttrs = mainClass.map(Name.MAIN_CLASS -> _).toList
          new Jar(source.file).jarWriter(jarMainAttrs: _*)
        else
          // Writing to non-empty JAR might be an undefined behaviour, e.g. in case if other files where
          // created using `AbstractFile.bufferedOutputStream`instead of JarWritter
          backendReporting.warning(em"Tried to write to non-empty JAR: $source")
          null
      }.orNull

    case _ => null
  }

  private def getDirectoryOrNull(dir: Option[String]): AbstractFile =
    dir.map(d => new PlainDirectory(Directory(d))).orNull

  private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    if (base.file != null) {
      fastGetFile(base, clsName, suffix)
    } else {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)
      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }
  }

  private def fastGetFile(base: AbstractFile, clsName: String, suffix: String) = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = base.file.toPath.resolve(packageName)
    new PlainFile(Path(directory.resolve(simpleName + suffix)))
  }

  private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
    if (outFile.file != null) {
      val outPath = outFile.file.toPath
      try Files.write(outPath, bytes)
      catch {
        case _: java.nio.file.NoSuchFileException =>
          Files.createDirectories(outPath.getParent)
          Files.write(outPath, bytes)
      }
    } else {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }
  }

  def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): AbstractFile | Null = try {
    // val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
    val outFile = writeToJarOrFile(className, bytes, ".class")
    // Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)

    if (dumpOutputDir != null) {
      val dumpFile = getFile(dumpOutputDir, className, ".class")
      writeBytes(dumpFile, bytes)
    }
    outFile
  } catch {
    case e: FileConflictException =>
      backendReporting.error(em"error writing $className: ${e.getMessage}")
      null
    case e: java.nio.file.FileSystemException =>
      if compilerSettings.debug then e.printStackTrace()
      backendReporting.error(em"error writing $className: ${e.getClass.getName} ${e.getMessage}")
      null
  }

  def writeTasty(className: InternalName, bytes: Array[Byte]): Unit =
    writeToJarOrFile(className, bytes, ".tasty")

  private def writeToJarOrFile(className: InternalName, bytes: Array[Byte], suffix: String): AbstractFile | Null = {
    if jarWriter == null then
      val outFolder = compilerSettings.outputDirectory
      val outFile = getFile(outFolder, className, suffix)
      try writeBytes(outFile, bytes)
      catch case ex: ClosedByInterruptException =>
        try outFile.delete() // don't leave an empty or half-written files around after an interrupt
        catch case _: Throwable => ()
        finally throw ex
      outFile
    else
      val path = className + suffix
      val out = jarWriter.newOutputStream(path)
      try out.write(bytes, 0, bytes.length)
      finally out.flush()
      null
  }

  def close(): Unit = {
    if (jarWriter != null) jarWriter.close()
  }
}

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)
