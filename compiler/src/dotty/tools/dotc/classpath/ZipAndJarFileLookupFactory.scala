/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc
package classpath

import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}

import dotty.tools.io.{AbstractFile, ClassPath, FileZipArchive}
import dotty.tools.dotc.core.Contexts.*
import FileUtils.*

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

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File, override val release: String)
    extends ZipArchiveFileLookup[BinaryFileEntry] {

    override def findClassFile(className: String): Option[AbstractFile] =
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(pkg, simpleClassName + ".class").map(_.file)

    override def classes(inPackage: String): Seq[BinaryFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): BinaryFileEntry = BinaryFileEntry(file)

    override protected def isRequiredFileType(file: AbstractFile): Boolean =
      file.exists && (file.ext.isTasty || (file.ext.isClass && !file.hasSiblingTasty))
  }

  override protected def createForZipFile(zipFile: AbstractFile, jFile: File | Null, release: String): ClassPath =
    if (jFile == null) throw new IllegalArgumentException(s"Abstract files which don't have an underlying file are not supported. There was $zipFile")
    else ZipArchiveClassPath(jFile, release)
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File, override val release: String) extends ZipArchiveFileLookup[SourceFileEntry] {
    override def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntry = SourceFileEntry(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.ext.isSourceExtension
  }

  override protected def createForZipFile(zipFile: AbstractFile, jFile: File | Null, release: String): ClassPath =
    assert(jFile != null, "Zip file in ZipAndJarSourcePathFactory cannot be null")
    ZipArchiveSourcePath(jFile, release)
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
