package dotty.tools.io

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.nio.file.{FileSystemAlreadyExistsException, FileSystems}
import java.util.jar.{Attributes, JarInputStream}
import scala.jdk.CollectionConverters.*

/**
 * This class implements an [[AbstractFile]] backed by a jar
 * that be can used as the compiler's output directory.
 */
class JarArchive private (val jarPath: Path, root: Directory) extends PlainDirectory(root) {
  def close(): Unit = this.synchronized(jpath.getFileSystem().close())
  override def exists: Boolean = jpath.getFileSystem().isOpen() && super.exists

  override def toString: String = jarPath.toString
}

object JarArchive {
  /** Create a new jar file. Overwrite if file already exists */
  def create(path: Path): JarArchive = {
    require(path.ext.isJar)
    path.delete()
    open(path, create = true)
  }

  /** Create a jar file. */
  def open(path: Path, create: Boolean = false): JarArchive = {
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

  // See http://download.java.net/jdk7/docs/api/java/nio/file/Path.html
  // for some ideas.
  private val ZipMagicNumber = List[Byte](80, 75, 3, 4)
  private def magicNumberIsZip(f: Path) = f.isFile && {
    val in = f.toFile.inputStream()
    try
      val first4 = in.readNBytes(4)
      first4.toList == ZipMagicNumber
    finally
      in.close()
  }

  def isJarOrZip(f: Path): Boolean =
    f.ext.isJarOrZip || magicNumberIsZip(f)

  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] =
    def specToURL(spec: String, basedir: Directory): Option[URL] =
      try
        val uri = new URI(spec)
        if uri.isAbsolute then Some(uri.toURL)
        else Some(basedir.resolve(Path(spec)).toURL)
      catch
        case _: MalformedURLException | _: URISyntaxException => None

    val file = File(jarPath)
    if !file.isFile then
      return Nil

    val baseDir = file.parent
    val in = new JarInputStream(file.inputStream())
    val manifest =
      try Option(in.getManifest)
      finally in.close()

    manifest match
      case None => Nil
      case Some(m) =>
        val attrs = m.getMainAttributes.asInstanceOf[java.util.Map[Attributes.Name, String]]
        attrs.get(Attributes.Name.CLASS_PATH) match
          case cp: String if cp.trim().nonEmpty =>
            cp.split("\\s+").toList.map(elem => specToURL(elem, baseDir).getOrElse((baseDir / elem).toURL))
          case _ => Nil
}
