package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream, SequenceInputStream}
import java.util.jar.*
import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala

// TODO JAR compression level ... ask it for every opening of a FileContainer in addition to jarVersion? depends on what the compiler needs

// TODO unify with ZipFile? only diff is having a version I think?

// General implementation note: ZipEntry.getName is what we'd call its path, not just its filename

object JarContainer:
  def open(file: File, version: String): FileContainer = file match
    case disk: DiskFile => new JarContainer(disk, version)
    // TODO: Use JarInputStream... but then we have to handle multi-versioning ourselves
    case _ => throw new UnsupportedOperationException("Loading JARs from outside the disk is not supported yet.")

private final class JarContainer private(val underlying: File, version: String) extends FileContainer, JarEntryContainerBase(""):
  lazy val jarFile = new JarFile(
    new java.io.File(underlying.path),
    true, // default
    java.util.zip.ZipFile.OPEN_READ, // default
    if version == "" then Runtime.version() else Runtime.Version.parse(version)
  )
  // While JarFile provides random access by name, there is no concept of "give me all the entries whose path starts with...",
  // so to avoid having to read a potentially large JAR many times, as well as re-creating many instances of our Container/File types, we cache entries.
  // This also allows us to support modification, though we must know whether anything was modified so we don't re-write JARs unnecessarily.
  var jarModified: Boolean = false
  lazy val jarEntries: mutable.Map[String, JarEntryFileSystemEntry] =
    jarFile.versionedStream().collect(Collectors.toMap(
      (e: JarEntry) => e.getName,
      (e: JarEntry) => if e.isDirectory then new JarEntryContainer(JarContainer.this, e) else new JarEntryFile(JarContainer.this, e)
    )).asScala

  override def name: String =
    underlying.name

  override def path: String =
    underlying.path

  override def parent: FileContainer =
    underlying.parent

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] =
    jarEntries.values

  override def deleteRecursively(): Unit =
    jarFile.close()
    underlying.delete()

  override def close(): Boolean = {
    if jarModified then
      val output = new JarOutputStream(underlying.output())
      for (_, entry) <- jarEntries do
        output.putNextEntry(entry.entry)
        entry match
          case f: File =>
            val in = f.input()
            try in.transferTo(output)
            finally in.close()
          case _ => ()
        output.closeEntry()
      output.close()
    jarFile.close()
    jarModified
  }

  // For JarEntryContainerBase
  override def container: JarContainer = this

private trait JarEntryFileSystemEntry(container: JarContainer, val entry: JarEntry) extends FileSystemEntry:
  def entryPath: String =
    entry.getName

  override def name: String =
    val idx = path.lastIndexOf('/')
    if idx == -1 then path else path.substring(idx + 1)

  override def path: String =
    container.path + "!" + entry.getName

  override def parent: FileContainer =
    val idx = entry.getName.lastIndexOf('/')
    if idx == 0 then
      container
    else
      val parentPath = entry.getName.substring(0, idx)
      val parentEntry = container.jarFile.getEntry(parentPath)
      new JarEntryContainer(container, entry)

  override def enclosing: Option[File] =
    Some(container.underlying)

private trait JarEntryContainerBase(entryPath: String) extends FileContainer:
  // This trait cannot take a JarContainer as parameter since JarContainer itself implements it and `this` cannot be a trait argument
  def container: JarContainer

  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    container.jarEntries.get(entryPath + name) match
      case Some(f: File) => Some(f)
      case _ => None

  protected override def getContainer(name: String): Option[FileContainer] =
    container.jarEntries.get(entryPath + name) match
      case Some(c: FileContainer) => Some(c)
      case _ => None

  protected override def createFile(name: String, extension: FileExtension): File = {
    container.jarModified = true
    val entry = new JarEntry(entryPath + name + extension.withDot)
    val result = new JarEntryFile(
      container,
      entry,
      modifiedInput = Some(InputStream.nullInputStream),
      modifiedSize = Some(0),
      modifiedTime = Some(System.currentTimeMillis())
    )
    container.jarEntries(entry.getName) = result
    result
  }

  protected override def createContainer(name: String): FileContainer = {
    container.jarModified = true
    val entry = new JarEntry(entryPath + name + "/")
    val result = new JarEntryContainer(container, entry)
    container.jarEntries(entry.getName) = result
    result
  }

private final class JarEntryContainer(override val container: JarContainer, entry: JarEntry) extends FileContainer, JarEntryFileSystemEntry(container, entry), JarEntryContainerBase(entry.getName):
  // `entries` is defined in terms of `recursiveEntries` since the latter is simpler as it needs no filtering
  override def entries: Iterable[FileSystemEntry] = {
    // An entry is a direct descendant if it either has no more '/' after this entry's path,
    // or if that '/' is at the end (because it's a directory)
    val start = entry.getName.length + 1
    def directDescendant(e: JarEntryFileSystemEntry): Boolean =
      val idx = e.entryPath.lastIndexOf('/', start)
      idx == -1 || idx == e.entryPath.length - 1
    recursiveEntries.filter(directDescendant)
  }

  // toList so modifications can be made during iteration
  override def recursiveEntries: Iterable[JarEntryFileSystemEntry] =
    container.jarEntries.values.toList.filter(e => e != this && e.entryPath.startsWith(entry.getName))

  override def deleteRecursively(): Unit =
    container.jarModified = true
    recursiveEntries.foreach(e => container.jarEntries.remove(e.entryPath))

private final class JarEntryFile(container: JarContainer, entry: JarEntry,
                                 var modifiedInput: Option[() => InputStream] = None,
                                 var modifiedSize: Option[Long] = None,
                                 var modifiedTime: Option[Long] = None) extends File, JarEntryFileSystemEntry(container, entry):
  override def lastModified(): Long =
    modifiedTime.getOrElse(entry.getLastModifiedTime.toMillis)

  override def size(): Long =
    modifiedSize.getOrElse(entry.getSize)

  override def input(): InputStream =
    modifiedInput.map(i => i()).getOrElse(container.jarFile.getInputStream(entry))

  override def output(append: Boolean = false): OutputStream = new ByteArrayOutputStream() {
    override def close(): Unit =
      container.jarModified = true
      val bytes = toByteArray
      val newStream = new ByteArrayInputStream(bytes)
      modifiedTime = Some(System.currentTimeMillis())
      if append then
        modifiedInput = Some(() => new SequenceInputStream(input(), newStream))
        modifiedSize = Some(JarEntryFile.this.size() + bytes.length)
      else
        modifiedInput = Some(() => newStream)
        modifiedSize = Some(bytes.length)
  }

  override def delete(): Unit =
    container.jarModified = true
    container.jarEntries.remove(entry.getName)
