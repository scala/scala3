package dotty.tools.io

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.nio.file.{FileSystemAlreadyExistsException, FileSystems, InvalidPathException, Paths}
import java.util.jar.{Attributes, JarInputStream}
import scala.jdk.CollectionConverters.*

private[tools] sealed trait JarDestination extends AbstractFile {
  private[io] def writableArchive: JarArchive
  def close(): Unit
}

/**
 * This class implements an [[AbstractFile]] backed by a jar
 * that be can used as the compiler's output directory.
 */
class JarArchive private (underlying: Path, root: Directory) extends PlainDirectory(root) with JarDestination {
  private[io] def writableArchive: JarArchive = this
  def close(): Unit = this.synchronized(jpath.getFileSystem().close())

  override val path: String = underlying.path
  override def ext: FileExtension = underlying.ext

  override def exists: Boolean = jpath.getFileSystem().isOpen() && super.exists

  def underlyingSource: AbstractFile =
    new PlainFile(underlying)
}

object JarArchive {
  private[tools] def output(path: Path): JarDestination = new Output(path)

  private final class Output(target: Path) extends PlainDirectory(target.toDirectory) with JarDestination {
    @volatile private var opened: JarArchive | Null = null

    private[io] def writableArchive: JarArchive = synchronized {
      dotty.tools.initialize(opened, opened = _, create(target))
    }

    override def iterator: Iterator[PlainFile] =
      Option(opened).iterator.flatMap(_.iterator)

    override def lookupName(name: String, directory: Boolean): AbstractFile | Null =
      if opened == null then null else opened.nn.lookupName(name, directory)

    override def fileNamed(name: String): AbstractFile = writableArchive.fileNamed(name)
    override def subdirectoryNamed(name: String): AbstractFile = writableArchive.subdirectoryNamed(name)

    def close(): Unit = synchronized {
      if opened != null then opened.nn.close()
    }
  }

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
