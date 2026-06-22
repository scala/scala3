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
import java.util.jar.Attributes
import java.util.zip.CRC32
import java.util.zip.Deflater
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

object FileWriters {
  private def classRelativePath(className: String, suffix: String): String =
    className.replace('.', '/') + suffix

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
    def writeTasty(name: String, bytes: Array[Byte]): AbstractFile

    /**
     * Close the writer. Behavior is undefined after a call to `close`.
     */
    def close(): Unit
  }

  object TastyWriter {
    def apply(output: AbstractFile): TastyWriter =
      // In Scala 2 depending on cardinality of distinct output dirs MultiClassWriter could have been used
      // In Dotty we always use single output directory
      new SingleTastyWriter(FileWriter(output, Seq.empty))

    private final class SingleTastyWriter(underlying: FileWriter) extends TastyWriter {

      override def writeTasty(className: String, bytes: Array[Byte]): AbstractFile = {
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
    def writeClass(name: String, bytes: Array[Byte]): AbstractFile

    /**
     * Close the writer. Behavior is undefined after a call to `close`.
     */
    def close(): Unit
  }

  object ClassfileWriter {
    def apply(output: AbstractFile, jarManifestMainClass: Option[String], jarCompressionLevel: Int, dumpClassesPath: Option[AbstractFile]): ClassfileWriter = {
      // In Scala 2 depending on cardinality of distinct output dirs MultiClassWriter could have been used
      // In Dotty we always use single output directory
      val manifest = jarManifestMainClass match
        case None => Seq.empty
        case Some(c) => Seq((Attributes.Name.MAIN_CLASS, c))

      val basicClassWriter = new SingleClassWriter(FileWriter(output, manifest, jarCompressionLevel))
      dumpClassesPath match
        case None => basicClassWriter
        case Some(out) => new DebugClassWriter(basicClassWriter, FileWriter(out, Seq.empty))
    }

    private final class SingleClassWriter(underlying: FileWriter) extends ClassfileWriter {
      override def writeClass(className: String, bytes: Array[Byte]): AbstractFile = {
        underlying.writeFile(classRelativePath(className, ".class"), bytes)
      }

      override def writeTasty(className: String, bytes: Array[Byte]): AbstractFile = {
        underlying.writeFile(classRelativePath(className, ".tasty"), bytes)
      }

      override def close(): Unit = underlying.close()
    }

    private final class DebugClassWriter(basic: ClassfileWriter, dump: FileWriter) extends ClassfileWriter {
      override def writeClass(className: String, bytes: Array[Byte]): AbstractFile = {
        val outFile = basic.writeClass(className, bytes)
        dump.writeFile(classRelativePath(className, ".class"), bytes)
        outFile
      }

      override def writeTasty(className: String, bytes: Array[Byte]): AbstractFile = {
        basic.writeTasty(className, bytes)
      }

      override def close(): Unit = {
        basic.close()
        dump.close()
      }
    }
  }

  sealed trait FileWriter {
    def writeFile(relativePath: String, bytes: Array[Byte]): AbstractFile
    def close(): Unit
  }

  object FileWriter {
    def apply(file: AbstractFile, jarManifest: Seq[(Attributes.Name, String)], jarCompressionLevel: Int = Deflater.DEFAULT_COMPRESSION): FileWriter =
      if file.isInstanceOf[JarArchive] then
        // Writing to non-empty JAR might be an undefined behaviour, e.g. in case if other files where
        // created using `AbstractFile.bufferedOutputStream` instead of JarWriter
        val jarFile = file.underlyingSource.getOrElse {
          throw new IllegalStateException("No underlying source for jar")
        }
        assert(file.isEmpty, s"Unsafe writing to non-empty JAR: $jarFile")
        new JarEntryWriter(jarFile, jarManifest, jarCompressionLevel)
      else if file.isVirtual then new VirtualFileWriter(file)
      else if file.isDirectory then new DirEntryWriter(file.file.nn.toPath)
      else throw new IllegalStateException(s"don't know how to handle an output of $file [${file.getClass}]")
  }

  private final class JarEntryWriter(file: AbstractFile, extraManifest: Seq[(Attributes.Name, String)], compressionLevel: Int) extends FileWriter {
    //keep these imports local - avoid confusion with scala naming
    import java.util.jar.Attributes.Name.MANIFEST_VERSION
    import java.util.jar.{JarOutputStream, Manifest}

    private val storeOnly = compressionLevel == Deflater.NO_COMPRESSION

    private val jarWriter: JarOutputStream = {
      import scala.util.Properties.*
      val manifest = new Manifest
      val attrs = manifest.getMainAttributes
      attrs.put(MANIFEST_VERSION, "1.0")
      attrs.put(ScalaCompilerVersion, versionNumberString)
      extraManifest.foreach { case (a, v) => attrs.put(a, v) }

      val jar = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
      jar.setLevel(compressionLevel)
      if (storeOnly) jar.setMethod(ZipOutputStream.STORED)
      jar
    }

    private lazy val crc = new CRC32

    override def writeFile(relativePath: String, bytes: Array[Byte]): AbstractFile = this.synchronized {
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
    import DirEntryWriter.*
    private val builtPaths = new ConcurrentHashMap[Path, java.lang.Boolean]()

    private def ensureDirForPath(baseDir: Path, filePath: Path): Unit = {
      import java.lang.Boolean.TRUE
      val parent = filePath.getParent
      if (!builtPaths.containsKey(parent)) {
        parent.iterator.forEachRemaining(checkName)
        try Files.createDirectories(parent, noAttributes*)
        catch {
          case e: FileAlreadyExistsException =>
            // `createDirectories` reports this exception if `parent` is an existing symlink to a directory
            // but that's fine for us (and common enough, `scalac -d /tmp` on Mac targets symlink).
            if (!Files.isDirectory(parent))
              throw new FileConflictException(s"Can't create directory $parent; there is an existing (non-directory) file in its path", e)
        }
        builtPaths.put(baseDir, TRUE)
        var current = parent
        while ((current ne null) && (null ne builtPaths.put(current, TRUE))) {
          current = current.getParent
        }
      }
      checkName(filePath.getFileName)
    }

    // the common case is that we are creating a new file, and on MS Windows the create and truncate is expensive
    // because there is not an options in the Windows API that corresponds to this so the truncate is applied as a separate call
    // even if the file is new.
    // as this is rare, it's best to always try to create a new file, and it that fails, then open with truncate if that fails
    private val fastOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    private val fallbackOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    override def writeFile(relativePath: String, bytes: Array[Byte]): AbstractFile = {
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
        case e: IOException => throw new IOException(s"Error writing $path: ${e.getClass.getName}: ${e.getMessage}", e)
      }
      AbstractFile.getFile(path).nn // we just wrote to it so it better still exist
    }

    override def close(): Unit = ()
  }

  private object DirEntryWriter {
    private val noAttributes = Array.empty[FileAttribute[?]]
    private val isWindows = scala.util.Properties.isWin
    private val windowsSpecialPaths = raw"(?i)CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9]".r

    // Otherwise users will get an obscure error when trying to create a file
    private def checkName(component: Path): Unit = if (isWindows) {
      val name = component.toString
      for
        prefix <- windowsSpecialPaths.findPrefixOf(name)
        if prefix.length == name.length || name(prefix.length) == '.'
      do
        throw new IOException(s"Path component is special Windows device: $name")
    }
  }

  private final class VirtualFileWriter(base: AbstractFile) extends FileWriter {
    private def getFile(base: AbstractFile, path: String): AbstractFile = {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$path: ${dir.path} is not a directory")
      val components = path.split('/')
      var dir = base
      for i <- 0 until components.length - 1 do
        dir = ensureDirectory(dir).subdirectoryNamed(components(i))
      ensureDirectory(dir).fileNamed(components.last)
    }

    private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }

    override def writeFile(relativePath: String, bytes: Array[Byte]): AbstractFile = {
      val outFile = getFile(base, relativePath)
      writeBytes(outFile, bytes)
      outFile
    }
    override def close(): Unit = ()
  }

  /** Can't output a file due to the state of the file system. */
  class FileConflictException(msg: String, cause: Throwable | Null = null) extends IOException(msg, cause)
}
