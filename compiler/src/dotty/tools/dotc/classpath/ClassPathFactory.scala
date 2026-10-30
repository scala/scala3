package dotty.tools.dotc.classpath

import dotty.tools.nio.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.interactive.LogicalSourcePath
import dotty.tools.dotc.interactive.LogicalPackage

import java.net.{URI, URISyntaxException}
import java.util.Locale
import java.util.jar.{Attributes, JarInputStream}

/*


/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {
  private val cache = new FileBasedCache[ClassPath]

  def create(zipFile: AbstractFile)(using Context): ClassPath =
    val release = ctx.settings.javaOutputVersion.value
    val jFile = zipFile.file
    if ctx.settings.YdisableFlatCpCaching.value || jFile == null then
      createForZipFile(zipFile, jFile, release)
    else
      cache.getOrCreate(jFile.toPath, () => createForZipFile(zipFile, jFile, release))

  protected def createForZipFile(zipFile: AbstractFile, jFile: File | Null, release: String): ClassPath

}

final class FileBasedCache[T] {
  private case class Stamp(lastModified: FileTime, fileKey: Object)
  private val cache = collection.mutable.Map.empty[java.nio.file.Path, (Stamp, T)]

  def getOrCreate(path: java.nio.file.Path, create: () => T): T = cache.synchronized {
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
    val lastModified = attrs.lastModifiedTime()
    // only null on some platforms, but that's okay, we just use the last modified timestamp as our stamp
    val fileKey = attrs.fileKey()
    val stamp = Stamp(lastModified, fileKey)
    cache.get(path) match {
      case Some((cachedStamp, cached)) if cachedStamp == stamp => cached
      case _ =>
        val value = create()
        cache.put(path, (stamp, value))
        value
    }
  }

  def clear(): Unit = cache.synchronized {
    // TODO support closing
    // cache.valuesIterator.foreach(_.close())
    cache.clear()
  }
}


* */


/**
 * Provides factory methods for classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
object ClassPathFactory {
  /**
   * Creators for sub classpaths which preserve this context.
   */
  def sourcesInPath(path: String, precomputedSourcePackages: Option[LogicalPackage])(using Context): List[ClassPath] =
    precomputedSourcePackages match
      // We also accept files in case of YlogicalPackageLoading
      case Some(rootPackage) if ctx.settings.YlogicalPackageLoading.value =>
        List(new LogicalSourcePath(path, rootPackage))
      case _ =>
        for
          entry <- ClassPath.expandPath(path, expandStar = false)
          root <- FileContainer.getOnDiskIncludingFromFile(entry, ctx.settings.javaOutputVersion.value, ctx.settings.XjarCompressionLevel.value)
        yield newClassPath(root)

  def contentsOfDirsInPath(path: String)(using Context): List[ClassPath] =
    ClassPath.expandPath(path, expandStar = false)
      .flatMap(FileContainer.getOnDisk)
      .flatMap(_.entries)
      .flatMap {
        case f: File => FileContainer.getFromFile(f, ctx.settings.javaOutputVersion.value, ctx.settings.XjarCompressionLevel.value)
        case c: FileContainer => Some(c)
      }.map(newClassPath)

  def classesInPath(path: String, expandStar: Boolean)(using Context): List[ClassPath] =
    val files: List[FileContainer] =
      for
        entry <- ClassPath.expandPath(path, expandStar)
        dir <- FileContainer.getOnDiskIncludingFromFile(entry, ctx.settings.javaOutputVersion.value, ctx.settings.XjarCompressionLevel.value)
      yield dir
    val expanded =
      if scala.util.Properties.propOrFalse("scala.expandjavacp") then
        for
          file <- files
          a <- expandManifestPath(file, ctx.settings.javaOutputVersion.value, ctx.settings.XjarCompressionLevel.value)
        yield
          newClassPath(a)
      else
        Seq.empty
    files.map(newClassPath) ++ expanded


  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  private def expandManifestPath(jarPath: FileContainer, jarVersion: String, jarCompressionLevel: Int): List[FileContainer] =
    def resolve(entry: String, baseDir: FileContainer): Option[FileContainer] =
      // Class-Path may only contain relative URLs, except for the 'file' scheme for compatibility reasons, per
      // https://docs.oracle.com/en/java/javase/17/docs/specs/jar/jar.html#class-path-attribute
      val uri =
        try new URI(entry)
        catch case _: URISyntaxException => return None
      if uri.getScheme == null then
        if uri.getPath.endsWith(".jar") then baseDir.getFile(uri.getPath).flatMap(FileContainer.getFromFile(_, jarVersion, jarCompressionLevel))
        else baseDir.getContainer(uri.getPath)
      else if uri.getScheme.toLowerCase(Locale.ROOT) == "file" then
        if uri.getPath.endsWith(".jar") then File.getOnDisk(uri.getPath).flatMap(FileContainer.getFromFile(_, jarVersion, jarCompressionLevel))
        else FileContainer.getOnDisk(uri.getPath)
      else
        None

    jarPath.enclosing match
      case None => Nil
      case Some(enclosing) =>
        val baseDir = enclosing.parent
        val in = new JarInputStream(enclosing.input())
        val manifest =
          try Option(in.getManifest)
          finally in.close()
        manifest match
          case None => Nil
          case Some(m) =>
            val attrs = m.getMainAttributes.asInstanceOf[java.util.Map[Attributes.Name, String]]
            attrs.get(Attributes.Name.CLASS_PATH) match
              case cp: String if cp.trim().nonEmpty =>
                cp.split("\\s+").toList.flatMap(elem => resolve(elem, baseDir))
              case _ => Nil

  def newClassPath(root: FileContainer): ClassPath =
    new FileContainerClassPath(root)
}
