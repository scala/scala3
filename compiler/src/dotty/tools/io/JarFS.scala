package dotty.tools
package io

import java.nio.file.{FileSystem, FileSystems}
import java.nio.file.Files

import java.util.jar._

import scala.collection.JavaConverters._

class JarFS private (private[this] var jarFS: FileSystem) {

  def getRoot(): AbstractFile = {
    val root = jarFS.getRootDirectories.iterator().next()
    new JarFS.JarDirectory(root)(jarFS)
  }

  def close() = {
    jarFS.close()
    jarFS = null
  }
}

object JarFS {

  def create(path: Path): JarFS = {
    assert(path.extension == "jar")
    val env = Map("create" -> "true").asJava
    val jarUri = java.net.URI.create("jar:file:" + path.toAbsolute.path)
    new JarFS(FileSystems.newFileSystem(jarUri, env))
  }

  private class JarDirectory(path: JPath)(jarFS: FileSystem) extends PlainDirectory(new Directory(path)) {
    override def name = "<jar-dir>"
    override def file = throw new UnsupportedOperationException
    override def fileNamed(name: String): AbstractFile = new JarFile(pathTo(name))
    override def subdirectoryNamed(name: String): AbstractFile = new JarDirectory(pathTo(name))(jarFS)
    private def pathTo(name: String): JPath =
      jarFS.getPath(if (path.toString == "/") name else path.resolve(name).toString)
  }

  private class JarFile(path: JPath) extends PlainFile(new File(path)) {
    override def name = "<jar-file>"
    override def file = throw new UnsupportedOperationException
    override def output = {
      if (path.getParent ne null)
        Files.createDirectories(path.getParent)
      Files.newOutputStream(path)
    }
  }
}
