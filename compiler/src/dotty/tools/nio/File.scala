package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.{BufferedReader, BufferedWriter, InputStream, InputStreamReader, OutputStream, OutputStreamWriter}
import scala.io.Codec

object File:
  /** Gets a file on disk at the given path, if it exists. */
  def getOnDisk(path: String): Option[File] =
    DiskFile.get(path)

  /** Gets or creates a file on disk at the given path. */
  def getOrCreateOnDisk(path: String): File =
    DiskFile.getOrCreate(path)

  /** If the given file is also a container, such as an archive, opens it as such and returns it, using the given JAR version if necessary. Otherwise, returns None. */
  def openAsArchive(file: File, jarVersion: String): Option[FileContainer] =
    if file.ext == FileExtension.Jar then Some(new JarContainer(file, jarVersion))
    else if file.ext == FileExtension.Zip then Some(new ZipContainer(file))
    else None

  /** Gets a file that does not exist, used to avoid the need for Option or nullables throughout the compiler. (This is kind a hack.) */
  def none(): File =
    NoFile

  // There is no "create an in-memory file" here. All files must have a container, so create a container first!

abstract class File extends FileSystemEntry:
  /** Extension of this file. */
  def ext: FileExtension

  /** Timestamp at which this file was last modified. Arbitrary but comparable across entries. */
  def lastModified: Long

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

  /** Opens an output stream to write into this file, optionally appending. Only use if other methods are not appropriate. */
  def output(append: Boolean = false): OutputStream

  /** Opens a writer to write into this file using the given codec, optionally appending. */
  def writer(codec: Codec, append: Boolean = false): BufferedWriter =
    new BufferedWriter(new OutputStreamWriter(output(append), codec.encoder))

  /** Writes the given bytes into this file, optionally appending. */
  def writeBytes(bytes: Array[Byte], append: Boolean = false): Unit =
    val out = output(append)
    try out.write(bytes)
    finally out.close()

  /** Writes the given text using the given codec into this file, optionally appending. */
  def writeText(str: String, codec: Codec, append: Boolean = false): Unit =
    val out = output(append)
    try out.write(str.getBytes(codec.charSet))
    finally out.close()

  /** Deletes this file. */
  def delete(): Unit