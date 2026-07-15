package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.{IOException, InputStream, OutputStream}

private[nio] object NoFile extends File:
  override def name: String = "<no file>"
  override def path: String = name
  override def parent: FileContainer = NoFileContainer
  override def enclosing: Option[File] = None
  override def extension: FileExtension = FileExtension.Empty
  override def lastModified(): Long = 0
  override def size(): Long = throw new IOException("NoFile has no size")
  override def input(): InputStream = throw new IOException("Cannot open NoFile for reading")
  override def output(append: Boolean = false): OutputStream = throw new IOException("Cannot open NoFile for writing")
  override def delete(): Unit = ()