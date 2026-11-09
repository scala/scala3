package dotty.tools.io

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.nio.file.{FileSystemAlreadyExistsException, FileSystems, InvalidPathException, Paths}
import java.util.jar.{Attributes, JarInputStream}
import scala.jdk.CollectionConverters.*

/**
 * This class implements an [[AbstractFile]] backed by a jar
 * that be can used as the compiler's output directory.
 */
class JarArchive private (underlying: Path, root: Directory) extends PlainDirectory(root) {
  def close(): Unit = this.synchronized(jpath.getFileSystem().close())

  override val path: String = underlying.path
  override def ext: FileExtension = underlying.ext

  override def exists: Boolean = jpath.getFileSystem().isOpen() && super.exists

  def underlyingSource: AbstractFile =
    new PlainFile(underlying)
}

object JarArchive {
  /** Create a new jar file. Overwrite if file already exists */
  def create(path: Path): JarArchive = {
    require(path.ext.isJar)
    path.delete()
    open(path, create = true)
  }

  /** Opens a jar file. */
  def open(path: Path): JarArchive =
    open(path, create = false)

  /** Opens or creates a jar file. */
  private def open(path: Path, create: Boolean): JarArchive = {
    require(path.ext.isJar)

    // creating a new zip file system by using the JAR URL syntax:
    // https://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html
    val env = Map("create" -> create.toString).asJava
    val uri = java.net.URI.create("jar:" + path.toAbsolute.toURI.toString)
    val fs = {
      try FileSystems.newFileSystem(uri, env)
      catch {
        case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri)
      }
    }
    val root = fs.getRootDirectories().iterator.next()
    new JarArchive(path, Directory(root))
  }
}
