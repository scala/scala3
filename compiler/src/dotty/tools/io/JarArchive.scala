package dotty.tools.io

import java.nio.file.{Path as JPath, *}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * This class implements an [[AbstractFile]] backed by a jar
 * that be can used as the compiler's output directory.
 */
class JarArchive private (root: Directory) extends PlainDirectory(root) {
  def close(): Unit = jpath.getFileSystem().close()

  def allFileNames(): Iterator[String] =
    Files.walk(jpath).iterator().asScala.map(_.toString)

  @volatile lazy val allDirs: scala.collection.Map[String, AbstractFile] = {
    val b = new mutable.HashMap[String, VirtualDirectory]
    for dir <- Files.walk(jpath).filter(Files.isDirectory(_)).iterator().asScala do
      b(dir.toString) = new VirtualDirectory(dir.getFileName.toString, b.get(dir.getParent.toString))
    b.result()
  }
}

object JarArchive {
  /** Create a new jar file. Overwrite if file already exists */
  def create(path: Path): JarArchive = {
    require(path.extension == "jar")
    path.delete()
    open(path, create = true)
  }

  /** Create a jar file. */
  def open(path: Path, create: Boolean = false): JarArchive = {
    require(path.extension == "jar")

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
    new JarArchive(Directory(root))
  }
}
