package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream, SequenceInputStream}
import java.util.jar.*
import java.util.stream.Collectors
import java.util.zip.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala

/* TODO: when we output a JAR from the compiler,
  manifest:
    Properties.ScalaCompilerVersion -> Properties.versionNumberString
 */

// General implementation note: ZipEntry.getName is what we'd call its path, not just its filename

object ZipContainer:
  def open(file: File, version: String, compressionLevel: Int, manifest: Map[String, String]): FileContainer = file match
    case disk: DiskFile => new ZipContainer(disk, version, compressionLevel, manifest)
    case _ => throw new UnsupportedOperationException("Only on-disk archives are supported.")

private final class ZipContainer private(val underlying: File, version: String, compressionLevel: Int, manifest: Map[String, String]) extends FileContainer, ZipEntryContainerBase(""):
  // ZipFile refuses to open 0-length files, so we must only load it if there is a nonempty file
  lazy val archive: Option[ZipFile] = Option.when(underlying.size() > 0)(
    if underlying.extension == FileExtension.Jar then
      new JarFile(
        new java.io.File(underlying.path),
        true, // default
        java.util.zip.ZipFile.OPEN_READ, // default
        if version == "" then Runtime.version() else Runtime.Version.parse(version)
      )
    else
      new ZipFile(new java.io.File(underlying.path))
  )
  // While ZipFile provides random access by name, there is no concept of "give me all the entries whose path starts with...",
  // so to avoid having to read a potentially large archive many times, as well as re-creating many instances of our Container/File types, we cache entries.
  // This also allows us to support modification, though we must know whether anything was modified so we don't re-write archives unnecessarily.
  var modified: Boolean = false
  lazy val rawEntries: mutable.Map[String, ZipEntryFileSystemEntry] = archive match
    case Some(jf) =>
      val stream = jf match
        case jar: JarFile => jar.versionedStream()
        case _ => jf.stream()
      stream.collect(Collectors.toMap(
        (e: ZipEntry) => e.getName,
        (e: ZipEntry) => if e.isDirectory then new ZipEntryContainer(ZipContainer.this, e)
                         else new ZipEntryFile(ZipContainer.this, e, inputs = mutable.ListBuffer(() => jf.getInputStream(e)))
      )).asScala
    case None => mutable.Map.empty

  override def name: String =
    underlying.name

  override def path: String =
    underlying.path

  override def parent: FileContainer =
    underlying.parent

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] =
    rawEntries.values

  override def deleteRecursively(): Unit =
    archive.foreach(_.close())
    underlying.delete()

  override def close(): Boolean = {
    if modified then
      val outputManifest = archive match
        case Some(jf: JarFile) => Some(jf.getManifest)
        case Some(jf) => None
        case None if underlying.extension == FileExtension.Jar =>
          // https://docs.oracle.com/en/java/javase/21/docs/specs/jar/jar.html#jar-manifest
          // hints that Manifest-Version is mandatory and must be first
          val m = new Manifest()
          m.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
          Some(m)
        case None => None
      outputManifest match
        case Some(m) => manifest.foreach((k, v) => m.getMainAttributes.putValue(k, v))
        case None => assert(manifest.isEmpty, "Manifests can only be emitted for JARs")
      // We can't write to `underlying` since we also (most likely) need to read from it
      File.createThenReplaceOnDisk(underlying.path) { newUnderlying =>
        val output = outputManifest match
          case Some(m) => new JarOutputStream(newUnderlying.output(), m)
          case None => new ZipOutputStream(newUnderlying.output())
        output.setLevel(compressionLevel)
        val crc = new CRC32()
        if compressionLevel == 0 then
          output.setMethod(ZipOutputStream.STORED)
        for (path, entry) <- rawEntries if path != "META-INF/MANIFEST.MF" do
          if compressionLevel == 0 then
            entry match
              case f: File =>
                val bytes = f.readBytes()
                // Java's ZIP API requires callers to set the size, compressed size, and CRC when using STORED
                entry.entry.setSize(f.size())
                entry.entry.setCompressedSize(f.size())
                crc.reset()
                crc.update(bytes)
                entry.entry.setCrc(crc.getValue)
                output.putNextEntry(entry.entry)
                output.write(bytes)
                output.closeEntry()
              case _ =>
                output.putNextEntry(entry.entry)
                output.closeEntry()
          else
            output.putNextEntry(entry.entry)
            entry match
              case f: File =>
                val in = f.input()
                try in.transferTo(output)
                finally in.close()
              case _ => ()
            output.closeEntry()
        output.close()
      }
    archive.foreach(_.close())
    modified
  }

  // For ZipEntryContainerBase
  override def container: ZipContainer = this

private trait ZipEntryFileSystemEntry(val container: ZipContainer, val entry: ZipEntry) extends FileSystemEntry:
  def entryPath: String =
    entry.getName

  override def name: String =
    val idx = path.lastIndexOf('/')
    if idx == -1 then path else path.substring(idx + 1)

  override def path: String =
    container.path + "!" + entry.getName

  override def parent: FileContainer =
    // directories end with a /, we want the previous one
    val idx = entry.getName.lastIndexOf('/', if entry.getName.last == '/' then entry.getName.length - 2 else entry.getName.length - 1)
    if idx == -1 then
      container
    else
      val parentPath = entry.getName.substring(0, idx)
      // OK to .get on archive here because since we have a file system entry the archive can't be empty
      val parentEntry = container.archive.get.getEntry(parentPath)
      new ZipEntryContainer(container, parentEntry)

  override def enclosing: Option[File] =
    Some(container.underlying)

  override def hashCode(): Int =
    entry.getName.hashCode()

  override def equals(obj: Any): Boolean = obj match
    case otherEntry: ZipEntryFileSystemEntry => container == otherEntry.container && entry.getName == otherEntry.entry.getName
    case _ => false

private trait ZipEntryContainerBase(entryPath: String) extends FileContainer:
  // This trait cannot take a ZipContainer as parameter since ZipContainer itself implements it and `this` cannot be a trait argument
  def container: ZipContainer

  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    container.rawEntries.get(entryPath + name + extension.withDot) match
      case Some(f: File) => Some(f)
      case _ => None

  protected override def getContainer(name: String): Option[FileContainer] =
    container.rawEntries.get(entryPath + name + "/") match
      case Some(c: FileContainer) => Some(c)
      case _ => None

  protected override def createFile(name: String, extension: FileExtension): File = {
    container.modified = true
    val entryName = entryPath + name + extension.withDot
    val entry =
      if container.underlying.extension == FileExtension.Jar
      then new JarEntry(entryName)
      else new ZipEntry(entryName)
    val result = new ZipEntryFile(
      container,
      entry,
      inputs = mutable.ListBuffer.empty,
      modifiedSize = Some(0),
      modifiedTime = Some(System.currentTimeMillis())
    )
    container.rawEntries(entry.getName) = result
    result
  }

  protected override def createContainer(name: String): FileContainer = {
    container.modified = true
    val entryName = entryPath + name + "/"
    val entry =
      if container.underlying.extension == FileExtension.Jar
      then new JarEntry(entryName)
      else new ZipEntry(entryName)
    val result = new ZipEntryContainer(container, entry)
    container.rawEntries(entry.getName) = result
    result
  }

private final class ZipEntryContainer(container: ZipContainer, entry: ZipEntry) extends FileContainer, ZipEntryFileSystemEntry(container, entry), ZipEntryContainerBase(entry.getName):
  // `entries` is defined in terms of `recursiveEntries` since the latter is simpler as it needs no filtering
  override def entries: Iterable[FileSystemEntry] = {
    // An entry is a direct descendant if it either has no more '/' after this entry's path,
    // or if that '/' is at the end (because it's a directory)
    val start = entry.getName.length
    def directDescendant(e: ZipEntryFileSystemEntry): Boolean =
      val idx = e.entryPath.indexOf('/', start)
      idx == -1 || idx == e.entryPath.length - 1
    recursiveEntries.filter(directDescendant)
  }

  // toList so modifications can be made during iteration
  override def recursiveEntries: Iterable[ZipEntryFileSystemEntry] =
    container.rawEntries.values.toList.filter(e => e != this && e.entryPath.startsWith(entry.getName))

  override def deleteRecursively(): Unit =
    container.modified = true
    recursiveEntries.foreach(e => container.rawEntries.remove(e.entryPath))

private final class ZipEntryFile(container: ZipContainer, entry: ZipEntry,
                                 var inputs: mutable.ListBuffer[() => InputStream],
                                 var modifiedSize: Option[Long] = None,
                                 var modifiedTime: Option[Long] = None) extends File, ZipEntryFileSystemEntry(container, entry):
  override def lastModified(): Long =
    modifiedTime.getOrElse(entry.getLastModifiedTime.toMillis)

  override def size(): Long =
    modifiedSize.getOrElse(entry.getSize)

  override def input(): InputStream =
    inputs.foldLeft(InputStream.nullInputStream)((a, b) => new SequenceInputStream(a, b()))

  override def output(append: Boolean = false): OutputStream = new ByteArrayOutputStream() {
    override def close(): Unit =
      container.modified = true
      val bytes = toByteArray
      modifiedTime = Some(System.currentTimeMillis())
      if append then
        inputs.addOne(() => new ByteArrayInputStream(bytes))
        modifiedSize = Some(ZipEntryFile.this.size() + bytes.length)
      else
        inputs = mutable.ListBuffer(() => new ByteArrayInputStream(bytes))
        modifiedSize = Some(bytes.length)
  }

  override def delete(): Unit =
    container.modified = true
    container.rawEntries.remove(entry.getName)
