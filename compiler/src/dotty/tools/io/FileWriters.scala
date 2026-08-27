package dotty.tools.io

import dotty.tools.io.AbstractFile
import dotty.tools.io.JarArchive
import dotty.tools.io.PlainFile

import java.io.BufferedOutputStream
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.ClosedByInterruptException
import java.nio.channels.FileChannel
import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths, StandardOpenOption}
import java.nio.file.attribute.FileAttribute
import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.CRC32
import java.util.zip.Deflater
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream
import scala.collection.mutable
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition}
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.report

import scala.annotation.constructorOnly
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean
import java.util.ConcurrentModificationException

object FileWriters {
  private def classRelativePath(className: String, suffix: String): String =
    className.replace('.', '/') + suffix

  inline def ctx(using ReadOnlyContext): ReadOnlyContext = summon[ReadOnlyContext]

  sealed trait DelayedReporter {
    def hasErrors: Boolean
    def error(message: Context ?=> Message, position: SourcePosition): Unit
    def warning(message: Context ?=> Message, position: SourcePosition): Unit
    def log(message: String): Unit

    final def toBuffered: Option[BufferingReporter] = this match
      case buffered: BufferingReporter =>
        if buffered.hasReports then Some(buffered) else None
      case _: EagerReporter => None

    def error(message: Context ?=> Message): Unit = error(message, NoSourcePosition)
    def warning(message: Context ?=> Message): Unit = warning(message, NoSourcePosition)
    final def exception(reason: Context ?=> Message, throwable: Throwable): Unit =
      error({
        val trace = throwable.getStackTrace().mkString("\n  ")
        em"An unhandled exception was thrown in the compiler while\n  ${reason.message}.\n${throwable}\n  $trace"
      }, NoSourcePosition)
  }

  final class EagerReporter(using captured: Context) extends DelayedReporter:
    private var _hasErrors = false

    def hasErrors: Boolean = _hasErrors

    def error(message: Context ?=> Message, position: SourcePosition): Unit =
      report.error(message, position)
      _hasErrors = true

    def warning(message: Context ?=> Message, position: SourcePosition): Unit =
      report.warning(message, position)

    def log(message: String): Unit = report.echo(message)

  enum Report:
    case Error(message: Context => Message, position: SourcePosition)
    case Warning(message: Context => Message, position: SourcePosition)
    case Log(message: String)

  final class BufferingReporter extends DelayedReporter {
    // We optimise access to the buffered reports for the common case - that there are no warning/errors to report
    // We could use a listBuffer etc - but that would be extra allocation in the common case
    // buffered logs are updated atomically.

    private val _bufferedReports = AtomicReference(List.empty[Report])
    private val _hasErrors = AtomicBoolean(false)


    /** Atomically record that an error occurred */
    private def recordError(): Unit =
      _hasErrors.set(true)

    /** Atomically add a report to the log */
    private def recordReport(report: Report): Unit =
      _bufferedReports.getAndUpdate(report :: _)

    /** atomically extract and clear the buffered reports, must only be called at a synchronization point. */
    def resetReports(): List[Report] =
      val curr = _bufferedReports.get()
      if curr.nonEmpty && !_bufferedReports.compareAndSet(curr, Nil) then
        throw ConcurrentModificationException("concurrent modification of buffered reports")
      else curr

    def hasErrors: Boolean = _hasErrors.get()
    def hasReports: Boolean = _bufferedReports.get().nonEmpty

    def error(message: Context ?=> Message, position: SourcePosition): Unit =
      recordReport(Report.Error({case given Context => message}, position))
      recordError()

    def warning(message: Context ?=> Message, position: SourcePosition): Unit =
      recordReport(Report.Warning({case given Context => message}, position))

    def log(message: String): Unit =
      recordReport(Report.Log(message))
  }

  trait ReadOnlySettings:
    def jarCompressionLevel: Int
    def debug: Boolean

  trait ReadOnlyRun:
    def suspendedAtTyperPhase: Boolean

  trait ReadOnlyContext:
    val run: ReadOnlyRun
    val settings: ReadOnlySettings
    val reporter: DelayedReporter

  trait BufferedReadOnlyContext extends ReadOnlyContext:
    val reporter: BufferingReporter

  object ReadOnlyContext:
    def readSettings(using ctx: Context): ReadOnlySettings = new:
      val jarCompressionLevel = ctx.settings.XjarCompressionLevel.value
      val debug = ctx.settings.Ydebug.value

    def readRun(using ctx: Context): ReadOnlyRun = new:
      val suspendedAtTyperPhase = ctx.run.nn.suspendedAtTyperPhase

    def buffered(using Context): BufferedReadOnlyContext = new:
      val settings = readSettings
      val reporter = BufferingReporter()
      val run = readRun

    def eager(using Context): ReadOnlyContext = new:
      val settings = readSettings
      val reporter = EagerReporter()
      val run = readRun

  /**
   * The interface to writing classfiles. GeneratedClassHandler calls these methods to generate the
   * directory and files that are created, and eventually calls `close` when the writing is complete.
   *
   * The companion object is responsible for constructing an appropriate and optimal implementation for
   * the supplied settings.
   *
   * Operations are threadsafe.
   */
  sealed trait TastyWriter {
    /**
     * Write a `.tasty` file.
     *
     * @param name the internal name of the class, e.g. "scala.Option"
     */
    def writeTasty(name: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile

    /**
     * Close the writer. Behavior is undefined after a call to `close`.
     */
    def close(): Unit
  }

  object TastyWriter {

    def apply(output: AbstractFile)(using ReadOnlyContext): TastyWriter =
      // In Scala 2 depending on cardinality of distinct output dirs MultiClassWriter could have been used
      // In Dotty we always use single output directory
      new SingleTastyWriter(FileWriter(output, None))

    private final class SingleTastyWriter(underlying: FileWriter) extends TastyWriter {

      override def writeTasty(className: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
        underlying.writeFile(classRelativePath(className, ".tasty"), bytes)
      }

      override def close(): Unit = underlying.close()
    }

  }


  /**
   * The interface to writing classfiles. GeneratedClassHandler calls these methods to generate the
   * directory and files that are created, and eventually calls `close` when the writing is complete.
   *
   * The companion object is responsible for constructing an appropriate and optimal implementation for
   * the supplied settings.
   *
   * Operations are threadsafe.
   */
  sealed trait ClassfileWriter extends TastyWriter {
    /**
     * Write a classfile
     */
    def writeClass(name: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile

    /**
     * Close the writer. Behavior is undefined after a call to `close`.
     */
    def close(): Unit
  }

  object ClassfileWriter {
    def apply(output: AbstractFile, jarManifestMainClass: Option[String], dumpClassesPath: Option[AbstractFile])(using ReadOnlyContext): ClassfileWriter = {
      // In Scala 2 depending on cardinality of distinct output dirs MultiClassWriter could have been used
      // In Dotty we always use single output directory
      val basicClassWriter = new SingleClassWriter(FileWriter(output, jarManifestMainClass))
      dumpClassesPath match
        case None => basicClassWriter
        case Some(out) => new DebugClassWriter(basicClassWriter, FileWriter(out, None))
    }

    private final class SingleClassWriter(underlying: FileWriter) extends ClassfileWriter {
      override def writeClass(className: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
        underlying.writeFile(classRelativePath(className, ".class"), bytes)
      }

      override def writeTasty(className: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
        underlying.writeFile(classRelativePath(className, ".tasty"), bytes)
      }

      override def close(): Unit = underlying.close()
    }

    private final class DebugClassWriter(basic: ClassfileWriter, dump: FileWriter) extends ClassfileWriter {
      override def writeClass(className: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
        val outFile = basic.writeClass(className, bytes)
        dump.writeFile(classRelativePath(className, ".class"), bytes)
        outFile
      }

      override def writeTasty(className: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
        basic.writeTasty(className, bytes)
      }

      override def close(): Unit = {
        basic.close()
        dump.close()
      }
    }
  }


  sealed trait FileWriter {
    def writeFile(relativePath: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile
    def close(): Unit
  }

  object FileWriter {
    def apply(file: AbstractFile, jarManifestMainClass: Option[String])(using ReadOnlyContext): FileWriter =
      if (file.isInstanceOf[JarArchive]) {
        val jarCompressionLevel = ctx.settings.jarCompressionLevel
        // Writing to non-empty JAR might be an undefined behaviour, e.g. in case if other files where
        // created using `AbstractFile.bufferedOutputStream`instead of JarWriter
        val jarFile = file.underlyingSource.getOrElse{
          throw new IllegalStateException("No underlying source for jar")
        }
        assert(file.isEmpty, s"Unsafe writing to non-empty JAR: $jarFile")
        new JarEntryWriter(jarFile, jarManifestMainClass, jarCompressionLevel)
      }
      else if (file.isVirtual) new VirtualFileWriter(file)
      else if (file.isDirectory) new DirEntryWriter(file.file.nn.toPath)
      else throw new IllegalStateException(s"don't know how to handle an output of $file [${file.getClass}]")
  }

  private final class JarEntryWriter(file: AbstractFile, mainClass: Option[String], compressionLevel: Int) extends FileWriter {
    //keep these imports local - avoid confusion with scala naming
    import java.util.jar.Attributes.Name.{MANIFEST_VERSION, MAIN_CLASS}
    import java.util.jar.{JarOutputStream, Manifest}

    val storeOnly = compressionLevel == Deflater.NO_COMPRESSION

    val jarWriter: JarOutputStream = {
      import scala.util.Properties.*
      val manifest = new Manifest
      val attrs = manifest.getMainAttributes
      attrs.put(MANIFEST_VERSION, "1.0")
      attrs.put(ScalaCompilerVersion, versionNumberString)
      mainClass.foreach(c => attrs.put(MAIN_CLASS, c))

      val jar = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
      jar.setLevel(compressionLevel)
      if (storeOnly) jar.setMethod(ZipOutputStream.STORED)
      jar
    }

    lazy val crc = new CRC32

    override def writeFile(relativePath: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = this.synchronized {
      val entry = new ZipEntry(relativePath)
      if (storeOnly) {
        // When using compression method `STORED`, the ZIP spec requires the CRC and compressed/
        // uncompressed sizes to be written before the data. The JarOutputStream could compute the
        // values while writing the data, but not patch them into the stream after the fact. So we
        // need to pre-compute them here. The compressed size is taken from size.
        // https://stackoverflow.com/questions/1206970/how-to-create-uncompressed-zip-archive-in-java/5868403
        // With compression method `DEFLATED` JarOutputStream computes and sets the values.
        entry.setSize(bytes.length)
        crc.reset()
        crc.update(bytes)
        entry.setCrc(crc.getValue)
      }
      jarWriter.putNextEntry(entry)
      try jarWriter.write(bytes, 0, bytes.length)
      finally jarWriter.flush()
      // important detail here, even on Windows, Zinc expects the separator within the jar
      // to be the system default, (even if in the actual jar file the entry always uses '/').
      // see https://github.com/sbt/zinc/blob/dcddc1f9cfe542d738582c43f4840e17c053ce81/internal/compiler-bridge/src/main/scala/xsbt/JarUtils.scala#L47
      val pathInJar =
        if java.io.File.separatorChar == '/' then relativePath
        else relativePath.replace('/', java.io.File.separatorChar)
      PlainFile.toPlainFile(Paths.get(s"${file.absolutePath}!$pathInJar"))
    }

    override def close(): Unit = this.synchronized(jarWriter.close())
  }

  private final class DirEntryWriter(base: Path) extends FileWriter {
    val builtPaths = new ConcurrentHashMap[Path, java.lang.Boolean]()
    val noAttributes = Array.empty[FileAttribute[?]]
    private val isWindows = scala.util.Properties.isWin

    private def checkName(component: Path)(using ReadOnlyContext): Unit = if (isWindows) {
      val specials = raw"(?i)CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9]".r
      val name = component.toString
      def warnSpecial(): Unit = ctx.reporter.warning(em"path component is special Windows device: ${name}")
      specials.findPrefixOf(name).foreach(prefix => if (prefix.length == name.length || name(prefix.length) == '.') warnSpecial())
    }

    def ensureDirForPath(baseDir: Path, filePath: Path)(using ReadOnlyContext): Unit = {
      import java.lang.Boolean.TRUE
      val parent = filePath.getParent
      if (!builtPaths.containsKey(parent)) {
        parent.iterator.forEachRemaining(checkName)
        try Files.createDirectories(parent, noAttributes*)
        catch {
          case e: FileAlreadyExistsException =>
            // `createDirectories` reports this exception if `parent` is an existing symlink to a directory
            // but that's fine for us (and common enough, `scalac -d /tmp` on mac targets symlink).
            if (!Files.isDirectory(parent))
              throw new FileConflictException(s"Can't create directory $parent; there is an existing (non-directory) file in its path", e)
        }
        builtPaths.put(baseDir, TRUE)
        var current = parent
        while ((current ne null) && (null ne builtPaths.put(current, TRUE))) {
          current = current.getParent
        }
      }
      checkName(filePath.getFileName())
    }

    // the common case is that we are creating a new file, and on MS Windows the create and truncate is expensive
    // because there is not an options in the windows API that corresponds to this so the truncate is applied as a separate call
    // even if the file is new.
    // as this is rare, it's best to always try to create a new file, and it that fails, then open with truncate if that fails

    private val fastOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    private val fallbackOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    override def writeFile(relativePath: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
      val path = base.resolve(relativePath)
      try {
        ensureDirForPath(base, path)
        val os = if (isWindows) {
          try FileChannel.open(path, fastOpenOptions)
          catch {
            case _: FileAlreadyExistsException => FileChannel.open(path, fallbackOpenOptions)
          }
        } else FileChannel.open(path, fallbackOpenOptions)

        try os.write(ByteBuffer.wrap(bytes), 0L)
        catch {
          case ex: ClosedByInterruptException =>
            try Files.deleteIfExists(path) // don't leave an empty of half-written classfile around after an interrupt
            catch { case _: java.io.IOException => () }
            throw ex
        }
        os.close()
      } catch {
        case e: FileConflictException =>
          ctx.reporter.error(em"error writing ${path.toString}: ${e.getMessage}")
        case e: java.nio.file.FileSystemException =>
          if (ctx.settings.debug) e.printStackTrace()
          ctx.reporter.error(em"error writing ${path.toString}: ${e.getClass.getName} ${e.getMessage}")
      }
      AbstractFile.getFile(path).nn // we just wrote to it so it better still exist
    }

    override def close(): Unit = ()
  }

  private final class VirtualFileWriter(base: AbstractFile) extends FileWriter {
    private def getFile(base: AbstractFile, path: String): AbstractFile = {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/${path}: ${dir.path} is not a directory")
      val components = path.split('/')
      var dir = base
      for i <- 0 until components.length - 1 do
        dir = ensureDirectory(dir).subdirectoryNamed(components(i).toString)
      ensureDirectory(dir).fileNamed(components.last.toString)
    }

    private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }

    override def writeFile(relativePath: String, bytes: Array[Byte])(using ReadOnlyContext): AbstractFile = {
      val outFile = getFile(base, relativePath)
      writeBytes(outFile, bytes)
      outFile
    }
    override def close(): Unit = ()
  }

  /** Can't output a file due to the state of the file system. */
  class FileConflictException(msg: String, cause: Throwable | Null = null) extends IOException(msg, cause)
}
