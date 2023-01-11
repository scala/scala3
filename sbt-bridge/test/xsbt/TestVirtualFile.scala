package xsbt

import xsbti.PathBasedFile
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.io.Codec

class TestVirtualFile(path: Path) extends PathBasedFile:
  override def contentHash(): Long = ???
  override def input(): java.io.InputStream = Files.newInputStream(path)
  override def id(): String = name()
  override def name(): String = path.toFile.getName
  override def names(): Array[String] = ???
  override def toPath(): Path = path
