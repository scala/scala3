package dotty.tools.nio

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import scala.io.Codec

private final class MemoryFile(override val parent: MemoryContainer,
                               override val name: String) extends File:
  // This class is optimized for the common case of creating a file from a string
  // and reading it as a string using the same codec.
  private var contents: (String, Codec) | Array[Byte] | Null = null

  override def path: String =
    parent.path + FileSystemEntry.separator + name

  override def enclosing: Option[File] =
    None

  override def lastModified(): Long =
    0 // we do not track this

  override def size(): Long = contents match
    case (s: String, c: Codec) =>
      -1 // TODO: remove size() ?
    case bs: Array[Byte] => bs.length
    case null => 0

  override def delete(): Unit =
    contents = null
    parent.files.remove(name)

  override def input(): InputStream =
    new ByteArrayInputStream(readBytes())

  override def readBytes(): Array[Byte] = contents match
    case (s: String, c: Codec) =>
      // TODO log that we're converting
      val converted = s.getBytes(c.charSet)
      contents = converted
      converted
    case bs: Array[Byte] => bs
    case null => Array.emptyByteArray

  override def readText(codec: Codec): String = contents match
    case (s: String, c: Codec) if c == codec => s
    case _ => super.readText(codec) // TODO log that we're converting

  override def readLines(codec: Codec): Iterable[String] = contents match
    case (s: String, c: Codec) if c == codec => s.split(System.lineSeparator())
    case _ => super.readLines(codec) // TODO log that we're converting

  override def output(append: Boolean): OutputStream = new ByteArrayOutputStream() {
    override def close(): Unit =
      super.close()
      if append then
        contents = readBytes() ++ toByteArray
      else
        contents = toByteArray
  }

  override def writeBytes(bytes: Array[Byte], append: Boolean): Unit =
    if append then
      contents = readBytes() ++ bytes
    else
      contents = bytes

  override def writeText(str: String, codec: Codec, append: Boolean = false): Unit = contents match
    case (s: String, c: Codec) if c == codec =>
      contents = if append then (s + str, c) else (str, c)
    case null =>
      contents = (str, codec)
    case _ =>
      super.writeText(str, codec, append) // TODO log that we're converting