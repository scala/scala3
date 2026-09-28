package dotty.tools.nio

import java.io.{BufferedReader, BufferedWriter, InputStream, InputStreamReader, OutputStream, OutputStreamWriter}
import scala.Conversion.into
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.io.Codec

// TODO we should find "leaks" ie undeleted files

object File:
  // There is no "create an in-memory file" here. All files must have a container, so create a container first!

  /** Gets a file on disk at the given path, if it exists. */
  def getOnDisk(path: String): Option[File] =
    DiskFile.get(path)

  /** Gets or creates a file on disk at the given path. */
  def getOrCreateOnDisk(path: String): File =
    DiskFile.getOrCreate(path)

  /** Creates a temporary file on disk, ending with the given name hint (that can thus be used to set the file extension if desired). */
  def createTemporaryOnDisk(nameHint: String): File =
    DiskFile.createTemporary(nameHint)

  /** Creates a temporary file on disk, perform the given action, then move it to the given path (useful if the action uses the file currently at that path). */
  def createThenReplaceOnDisk(path: String)(action: File => Unit): Unit =
    val temp = createTemporaryOnDisk("partial")
    action(temp)
    DiskFile.moveTo(temp, path)

  private val EmptyExtension = FileExtension("")

abstract class File extends FileSystemEntry:
  /** Name of the file without the extension nor the period. */
  def nameWithoutExtension: String =
    val idx = name.lastIndexOf('.')
    if idx == -1 then name else name.substring(0, idx)

  /** Extension of this file. */
  def extension: FileExtension =
    val idx = name.lastIndexOf('.')
    if idx == -1 then File.EmptyExtension else FileExtension(name.substring(idx + 1))

  /** Timestamp at which this file was last modified. Arbitrary but comparable across entries. */
  def lastModified(): Long

  /** Size of the file in bytes. */
  def size(): Long

  /** Gets a sibling to this file with the same name but a different extension, if it exists. */
  def getSiblingWithExtension(ext: into[FileExtension]): Option[File] =
    parent.getFile(nameWithoutExtension + ext.withDot)

  /** Gets or creates a sibling to this file with the same name but a different extension. */
  def getOrCreateSiblingWithExtension(ext: into[FileExtension]): File =
    parent.getOrCreateFile(nameWithoutExtension + ext.withDot)

  /** Opens an input stream to read from this file. Only use if other methods are not appropriate. */
  def input(): InputStream

  /** Opens a reader to read from this file using the given codec. */
  def reader(codec: Codec): BufferedReader =
    new BufferedReader(new InputStreamReader(input(), codec.decoder))

  /** Reads all bytes from this file into a new array. */
  def readBytes(): Array[Byte] =
    val in = input()
    try in.readAllBytes()
    finally in.close()

  /** Reads all text from this file using the given codec. */
  def readText(codec: Codec): String =
    new String(readBytes(), codec.charSet)

  /** Reads all lines from this file using the given codec. */
  def readLines(codec: Codec): Iterable[String] =
    val r = reader(codec)
    try r.lines().iterator().asScala.toList
    finally r.close()

  /** Opens an output stream to write into this file, optionally appending. Only use if other methods are not appropriate. */
  def output(append: Boolean = false): OutputStream

  /** Opens a writer to write into this file using the given codec, optionally appending. */
  def writer(codec: Codec, append: Boolean = false): BufferedWriter =
    new BufferedWriter(new OutputStreamWriter(output(append), codec.encoder))

  /** Writes the given bytes into this file, optionally appending. */
  def writeBytes(bytes: Array[Byte], append: Boolean = false): Unit =
    val out = output(append)
    try out.write(bytes)
    catch case _: Exception => delete() // do not leave a half-written file after an exception
    finally out.close()

  /** Writes the given text using the given codec into this file, optionally appending. */
  def writeText(str: String, codec: Codec, append: Boolean = false): Unit =
    val out = output(append)
    try out.write(str.getBytes(codec.charSet))
    catch case _: Exception => delete() // do not leave a half-written file after an exception
    finally out.close()

  /** Writes the given lines using the given codec into this file, optionally appending. */
  def writeLines(lines: Iterable[String], codec: Codec, append: Boolean = false): Unit =
    writeText(lines.mkString(System.lineSeparator()) + System.lineSeparator(), codec, append)

  /** Copies this file to the given file. */
  def copyTo(other: File): Unit =
    val in = input()
    val out = other.output()
    try
      in.transferTo(out)
    finally
      in.close()
      out.close()

  /** Deletes this file. */
  def delete(): Unit