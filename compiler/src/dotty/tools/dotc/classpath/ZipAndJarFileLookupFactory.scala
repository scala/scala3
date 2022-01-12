/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc
package classpath

import java.io.File
import java.net.URL
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}

import scala.annotation.tailrec
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation}
import dotty.tools.dotc.core.Contexts._
import FileUtils._
import util._

/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {
  private val cache = new FileBasedCache[ClassPath]

  def create(zipFile: AbstractFile)(using Context): ClassPath =
    val release = Option(ctx.settings.release.value).filter(_.nonEmpty)
    if (ctx.settings.YdisableFlatCpCaching.value || zipFile.file == null) createForZipFile(zipFile, release)
    else createUsingCache(zipFile, release)

  protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath

  private def createUsingCache(zipFile: AbstractFile, release: Option[String]): ClassPath =
    cache.getOrCreate(zipFile.file.toPath, () => createForZipFile(zipFile, release))
}

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File, override val release: Option[String])
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(PackageName(pkg), simpleClassName + ".class").map(_.file)
    }

    // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
    override def findClass(className: String): Option[ClassRepresentation] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(PackageName(pkg), simpleClassName + ".class")
    }

    override private[dotty] def classes(inPackage: PackageName): Seq[ClassFileEntry] = files(inPackage)

    override protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
  }

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath =
    if (zipFile.file == null) createWithoutUnderlyingFile(zipFile)
    else ZipArchiveClassPath(zipFile.file, release)

  private def createWithoutUnderlyingFile(zipFile: AbstractFile) =
    val errorMsg = s"Abstract files which don't have an underlying file and are not ManifestResources are not supported. There was $zipFile"
    throw new IllegalArgumentException(errorMsg)
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {
    
    def release: Option[String] = None

    override def asSourcePathString: String = asClassPathString

    override private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath = ZipArchiveSourcePath(zipFile.file)
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
