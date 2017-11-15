package dotty.tools
package io

import java.nio.file.{FileSystem, FileSystems}
import java.nio.file.Files

import java.util.jar._

import scala.collection.JavaConverters._

class JarFS private (private[this] var jarFS: FileSystem) {

  def getRoot(): AbstractFile = {
    val root = jarFS.getRootDirectories.iterator().next()
    new PlainDirectory(Directory(root))
  }

  def close() = jarFS.close()
}

object JarFS {
  def create(path: Path): JarFS = {
    assert(path.extension == "jar")
    val env = Map("create" -> "true").asJava
    val jarUri = java.net.URI.create("jar:file:" + path.toAbsolute.path)
    new JarFS(FileSystems.newFileSystem(jarUri, env))
  }
}
