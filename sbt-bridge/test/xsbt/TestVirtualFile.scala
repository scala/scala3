package xsbt

import xsbti.{PathBasedFile, VirtualFileRef}
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.io.Codec

class TestVirtualFile(path: Path) extends PathBasedFile:
  override def contentHash(): Long = ???
  override def input(): java.io.InputStream = Files.newInputStream(path)
  lazy val absolutePath: String = path.toAbsolutePath.toString()
  override def id(): String = absolutePath
  override def name(): String = path.toFile.getName
  override def names(): Array[String] = ???
  override def toPath(): Path = path


  override def hashCode(): Int = absolutePath.hashCode()

  override def equals(x: Any): Boolean = this.eq(x.asInstanceOf[AnyRef]) || x.match {
    case vf: VirtualFileRef => vf.id() == id()
  }


