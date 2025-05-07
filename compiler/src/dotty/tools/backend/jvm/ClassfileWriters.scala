package dotty.tools.backend.jvm

import java.io.{DataOutputStream, File, IOException, BufferedOutputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{ClosedByInterruptException, FileChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.*
import java.nio.file.attribute.FileAttribute
import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{CRC32, Deflater, ZipEntry, ZipOutputStream}

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.util.chaining.*
import dotty.tools.io.{AbstractFile, PlainFile, VirtualFile}
import dotty.tools.io.PlainFile.toPlainFile
import BTypes.InternalName
import dotty.tools.io.JarArchive

import scala.language.unsafeNulls

/** !!! This file is now copied in `dotty.tools.io.FileWriters` in a more general way that does not rely upon
 * `PostProcessorFrontendAccess`, this should probably be changed to wrap that class instead.
 *
 * Until then, any changes to this file should be copied to `dotty.tools.io.FileWriters` as well.
 */
class ClassfileWriters(frontendAccess: PostProcessorFrontendAccess) {
  import frontendAccess.{compilerSettings, backendReporting}

  sealed trait TastyWriter {
    def writeTasty(name: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit
  }

  /**
   * The interface to writing classfiles. GeneratedClassHandler calls these methods to generate the
   * directory and files that are created, and eventually calls `close` when the writing is complete.
   *
   * The companion object is responsible for constructing a appropriate and optimal implementation for
   * the supplied settings.
   *
   * Operations are threadsafe.
   */
  sealed trait ClassfileWriter extends TastyWriter {
    /**
     * Write a classfile
     */
    def writeClass(name: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): AbstractFile


    /**
     * Close the writer. Behavior is undefined after a call to `close`.
     */
    def close(): Unit

    protected def classRelativePath(className: InternalName, suffix: String = ".class"): String =
      className.replace('.', '/').nn + suffix
  }

  object ClassfileWriter {
    private def getDirectory(dir: String): Path = Paths.get(dir).nn

    def apply(): ClassfileWriter = {
      val jarManifestMainClass: Option[String] = compilerSettings.mainClass.orElse {
        frontendAccess.getEntryPoints match {
          case List(name) => Some(name)
          case es =>
            if es.isEmpty then backendReporting.log("No Main-Class designated or discovered.")
            else backendReporting.log(s"No Main-Class due to multiple entry points:\n  ${es.mkString("\n  ")}")
            None
        }
      }

      // In Scala 2 depenening on cardinality of distinct output dirs MultiClassWriter could have been used
      // In Dotty we always use single output directory
      val basicClassWriter = new SingleClassWriter(
        FileWriter(compilerSettings.outputDirectory, jarManifestMainClass)
      )

      val withAdditionalFormats =
        compilerSettings.dumpClassesDirectory
          .map(getDirectory)
          .filter{path => Files.exists(path).tap{ok => if !ok then backendReporting.error(em"Output dir does not exist: ${path.toString}")}}
          .map(out => FileWriter(out.toPlainFile, None))
          .fold[ClassfileWriter](basicClassWriter)(new DebugClassWriter(basicClassWriter, _))

      // val enableStats = settings.areStatisticsEnabled && settings.YaddBackendThreads.value == 1
      // if (enableStats) new WithStatsWriter(withAdditionalFormats) else
      withAdditionalFormats
    }

    private final class SingleClassWriter(underlying: FileWriter) extends ClassfileWriter {
      override def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): AbstractFile = {
        underlying.writeFile(classRelativePath(className), bytes)
      }
      override def writeTasty(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
        underlying.writeFile(classRelativePath(className, ".tasty"), bytes)
      }


      override def close(): Unit = underlying.close()
    }

    private final class DebugClassWriter(basic: ClassfileWriter, dump: FileWriter) extends ClassfileWriter {
      override def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): AbstractFile = {
        val outFile = basic.writeClass(className, bytes, sourceFile)
        dump.writeFile(classRelativePath(className), bytes)
        outFile
      }

      override def writeTasty(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
        basic.writeTasty(className, bytes, sourceFile)
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
    def apply(file: AbstractFile, jarManifestMainClass: Option[String]): FileWriter =
      if (file.isInstanceOf[JarArchive]) {
        val jarCompressionLevel = compilerSettings.jarCompressionLevel
        // Writing to non-empty JAR might be an undefined behaviour, e.g. in case if other files where
        // created using `AbstractFile.bufferedOutputStream`instead of JarWriter
        val jarFile = file.underlyingSource.getOrElse{
          throw new IllegalStateException("No underlying source for jar")
        }
        assert(file.isEmpty, s"Unsafe writing to non-empty JAR: $jarFile")
        new JarEntryWriter(jarFile, jarManifestMainClass, jarCompressionLevel)
      }
      else if (file.isVirtual) new VirtualFileWriter(file)
      else if (file.isDirectory) new DirEntryWriter(file.file.toPath.nn)
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
      val attrs = manifest.getMainAttributes.nn
      attrs.put(MANIFEST_VERSION, "1.0")
      attrs.put(ScalaCompilerVersion, versionNumberString)
      mainClass.foreach(c => attrs.put(MAIN_CLASS, c))

      val jar = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
      jar.setLevel(compressionLevel)
      if (storeOnly) jar.setMethod(ZipOutputStream.STORED)
      jar
    }

    lazy val crc = new CRC32

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
        if File.separatorChar == '/' then relativePath
        else relativePath.replace('/', File.separatorChar)
      PlainFile.toPlainFile(Paths.get(s"${file.absolutePath}!$pathInJar"))
    }

    override def close(): Unit = this.synchronized(jarWriter.close())
  }

  private final class DirEntryWriter(base: Path) extends FileWriter {
    val builtPaths = new ConcurrentHashMap[Path, java.lang.Boolean]()
    val noAttributes = Array.empty[FileAttribute[?]]
    private val isWindows = scala.util.Properties.isWin

    private def checkName(component: Path): Unit = if (isWindows) {
      val specials = raw"(?i)CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9]".r
      val name = component.toString
      def warnSpecial(): Unit = backendReporting.warning(em"path component is special Windows device: ${name}")
      specials.findPrefixOf(name).foreach(prefix => if (prefix.length == name.length || name(prefix.length) == '.') warnSpecial())
    }

    def ensureDirForPath(baseDir: Path, filePath: Path): Unit = {
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

    // the common case is that we are are creating a new file, and on MS Windows the create and truncate is expensive
    // because there is not an options in the windows API that corresponds to this so the truncate is applied as a separate call
    // even if the file is new.
    // as this is rare, its best to always try to create a new file, and it that fails, then open with truncate if that fails

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
            try Files.deleteIfExists(path) // don't leave a empty of half-written classfile around after an interrupt
            catch { case _: Throwable => () }
            throw ex
        }
        os.close()
      } catch {
        case e: FileConflictException =>
          backendReporting.error(em"error writing ${path.toString}: ${e.getMessage}")
        case e: java.nio.file.FileSystemException =>
          if (compilerSettings.debug) e.printStackTrace()
          backendReporting.error(em"error writing ${path.toString}: ${e.getClass.getName} ${e.getMessage}")
      }
      AbstractFile.getFile(path)
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
      for (i <- 0 until components.length - 1) dir = ensureDirectory(dir) subdirectoryNamed components(i).toString
      ensureDirectory(dir) fileNamed components.last.toString
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
  class FileConflictException(msg: String, cause: Throwable = null) extends IOException(msg, cause)
}
