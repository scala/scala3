package dotty.tools.nio

import java.io.{BufferedReader, BufferedWriter, InputStream, OutputStream}
import java.nio.file.{OpenOption, StandardOpenOption, Files as JFiles, Path as JPath}
import dotty.tools.io.FileExtension

import scala.io.Codec

private[nio] object DiskFile:
  def get(path: String): Option[File] =
    val jpath = JPath.of(path)
    Option.when(JFiles.isRegularFile(jpath))(new DiskFile(jpath))

  def getOrCreate(path: String): File =
    val jpath = JPath.of(path)
    if !JFiles.isRegularFile(jpath) then {
      JFiles.createDirectories(jpath.getParent)
      JFiles.createFile(jpath)
    }
    new DiskFile(jpath)

  def createTemporary(nameHint: String, extension: FileExtension): File =
    new DiskFile(JFiles.createTempFile(nameHint, extension.withDot))

  private val appendOptions: Array[OpenOption] = Array(StandardOpenOption.APPEND, StandardOpenOption.CREATE)
  private val overwriteOptions: Array[OpenOption] = Array(StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)
  private def options(append: Boolean): Array[OpenOption] =
    if append then appendOptions else overwriteOptions

private[nio] final class DiskFile(underlying: JPath) extends File:
  override val name: String =
    underlying.getFileName.toString

  override val extension: FileExtension = {
    val idx = name.lastIndexOf('.')
    if idx == -1 then FileExtension.Empty else FileExtension.from(name.substring(idx))
  }

  override def path: String =
    underlying.toString

  override def parent: FileContainer =
    new DiskDirectory(underlying.getParent)

  override def enclosing: Option[File] =
    None

  override def lastModified: Long =
    JFiles.getLastModifiedTime(underlying).toMillis

  override def delete(): Unit =
    JFiles.deleteIfExists(underlying)

  override def input(): InputStream =
    JFiles.newInputStream(underlying)

  override def reader(codec: Codec): BufferedReader =
    JFiles.newBufferedReader(underlying, codec.charSet)

  override def readText(codec: Codec): String =
    JFiles.readString(underlying, codec.charSet)

  override def output(append: Boolean): OutputStream =
    JFiles.newOutputStream(underlying, DiskFile.options(append)*)

  override def writer(codec: Codec, append: Boolean): BufferedWriter =
    JFiles.newBufferedWriter(underlying, codec.charSet, DiskFile.options(append)*)

  override def writeText(str: String, codec: Codec, append: Boolean): Unit =
    JFiles.writeString(underlying, str, codec.charSet, DiskFile.options(append)*)