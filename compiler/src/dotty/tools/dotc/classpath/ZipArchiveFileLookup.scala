/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath


import java.io.File
import java.net.URL

import dotty.tools.io.{ AbstractFile, FileZipArchive }
import FileUtils.*
import dotty.tools.io.ClassPath

/**
 * A trait allowing to look for classpath entries of given type in zip and jar files.
 * It provides common logic for classes handling class and source files.
 * It's aware of things like e.g. META-INF directory which is correctly skipped.
 */
trait ZipArchiveFileLookup[FileEntryType] extends ClassPath {
  val zipFile: File
  val release: String

  override def asURLs: Seq[URL] = Seq(zipFile.toURI.toURL)

  private val archive = AbstractFile.getDirectory(zipFile.toPath, release).nn

  override def packages(inPackage: String): Seq[PackageEntry] =
    findDirEntry(inPackage) match {
      case None =>
        Seq.empty
      case Some(dirEntry) =>
        dirEntry.iterator.filter(_.isPackage).map(e => PackageEntry(PackageNameUtils.entryName(inPackage, e.name))).toSeq
    }

  protected def files(inPackage: String): Seq[FileEntryType] =
    findDirEntry(inPackage) match {
      case None =>
        Seq.empty
      case Some(dirEntry) =>
        dirEntry.iterator.filter(isRequiredFileType).map(createFileEntry).toSeq
    }

  protected def file(className: String): Option[FileEntryType] =
    // no "isRequiredFileType" filter, we know exactly what we want
    archive.lookupPath(className, '.', lastSuffix = ".class", directory = false).map(createFileEntry)

  override def hasPackage(pkg: String) = findDirEntry(pkg).isDefined

  private def findDirEntry(pkg: String): Option[AbstractFile] = {
    archive.lookupPath(pkg, '.', directory = true)
  }

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
}
