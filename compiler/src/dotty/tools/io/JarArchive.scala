package dotty.tools.io

import java.nio.file.{Files, FileSystem, FileSystems}

import scala.collection.JavaConverters._

/**
 * This class implements an [[AbstractFile]] backed by a jar
 * that be can used as the compiler's output directory.
 */
class JarArchive private (root: Directory) extends PlainDirectory(root) {
  def close() = jpath.getFileSystem().close()
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
    val uri = java.net.URI.create("jar:file:" + path.toAbsolute.path)
    val fs = FileSystems.newFileSystem(uri, env)

    val root = fs.getRootDirectories().iterator.next()
    new JarArchive(Directory(root))
  }
}
