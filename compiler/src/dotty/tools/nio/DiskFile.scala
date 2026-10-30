package dotty.tools.nio

import java.io.{BufferedReader, BufferedWriter, InputStream, OutputStream}
import java.nio.file.{FileAlreadyExistsException, OpenOption, StandardCopyOption, StandardOpenOption, Files as JFiles, Path as JPath}

import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.io.Codec

private object DiskFile:
  def get(path: String): Option[File] =
    val jpath = JPath.of(path).normalize()
    Option.when(JFiles.isRegularFile(jpath))(new DiskFile(jpath))

  def getOrCreate(path: String): File =
    val jpath = JPath.of(path).normalize()
    if !JFiles.isRegularFile(jpath) then
      JFiles.createDirectories(jpath.getParent)
      try JFiles.createFile(jpath)
      catch case e: FileAlreadyExistsException => throw new IllegalArgumentException(s"$path exists but is not a file")
    new DiskFile(jpath)

  def createTemporary(nameHint: String): File =
    new DiskFile(JFiles.createTempFile("", nameHint))

  def moveTo(file: File, path: String): Unit = file match
    case df: DiskFile => JFiles.move(JPath.of(df.path), JPath.of(path), StandardCopyOption.REPLACE_EXISTING)
    case _ => throw new IllegalArgumentException("Not a disk file, cannot move it")

  // We explicitly delete the existing file when not appending,
  // to ensure we do _not_ follow symlinks. E.g., if a symlink `foo/bar` exists to `baz`
  // and we want to write without appending to `foo/bar`, we must replace the symlink with a file, not replace `baz`
  private val options: Array[OpenOption] = Array(StandardOpenOption.APPEND, StandardOpenOption.CREATE)

// Invariant: `underlying` is normalized
private final class DiskFile(private val underlying: JPath) extends File:
  override def name: String =
    underlying.getFileName.toString

  override def path: String =
    underlying.toString

  override def parent: FileContainer =
    new DiskDirectory(underlying.getParent)

  override def enclosing: Option[File] =
    None

  override def lastModified(): Long =
    JFiles.getLastModifiedTime(underlying).toMillis

  override def size(): Long =
    JFiles.size(underlying)

  override def delete(): Unit =
    JFiles.deleteIfExists(underlying)

  override def input(): InputStream =
    JFiles.newInputStream(underlying)

  override def reader(codec: Codec): BufferedReader =
    JFiles.newBufferedReader(underlying, codec.charSet)

  override def readText(codec: Codec): String =
    JFiles.readString(underlying, codec.charSet)

  override def readLines(codec: Codec): Iterable[String] =
    JFiles.readAllLines(underlying, codec.charSet).asScala

  override def output(append: Boolean): OutputStream =
    if !append then JFiles.deleteIfExists(underlying)
    JFiles.newOutputStream(underlying, DiskFile.options*)

  override def writer(codec: Codec, append: Boolean): BufferedWriter =
    if !append then JFiles.deleteIfExists(underlying)
    JFiles.newBufferedWriter(underlying, codec.charSet, DiskFile.options*)

  override def writeText(str: String, codec: Codec, append: Boolean): Unit =
    if !append then JFiles.deleteIfExists(underlying)
    JFiles.writeString(underlying, str, codec.charSet, DiskFile.options*)

  override def copyTo(other: File): Unit = other match
    case other if this == other => ()
    case otherDiskFile: DiskFile =>
      JFiles.deleteIfExists(otherDiskFile.underlying)
      JFiles.copy(underlying, otherDiskFile.underlying)
    case _ => super.copyTo(other)

  override def hashCode(): Int =
    underlying.hashCode()

  override def equals(obj: Any): Boolean = obj match
    case otherDiskFile: DiskFile => underlying.equals(otherDiskFile.underlying)
    case _ => false