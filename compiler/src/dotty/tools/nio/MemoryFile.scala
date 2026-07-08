package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}

private[nio] final class MemoryFile(override val parent: FileContainer, override val name: String, override val ext: FileExtension) extends File:
  private var contents: Array[Byte] = Array.emptyByteArray

  override def path: String =
    parent.path + FileSystemEntry.separator + name

  override def enclosing: Option[File] =
    None

  override def lastModified: Long =
    0 // we do not track this

  override def delete(): Unit =
    contents = Array.emptyByteArray

  override def input(): InputStream =
    new ByteArrayInputStream(contents)

  override def output(append: Boolean): OutputStream = new ByteArrayOutputStream() {
    override def close(): Unit =
      super.close()
      contents = toByteArray
  }