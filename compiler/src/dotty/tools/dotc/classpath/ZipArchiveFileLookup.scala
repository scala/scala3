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

  private val archive = new FileZipArchive(zipFile.toPath, Some(release))

  override def packages(inPackage: String): Seq[PackageEntry] = {
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if entry.isPackage
    }
    yield PackageEntry(PackageNameUtils.entryName(inPackage, entry.name))
  }

  protected def files(inPackage: String): Seq[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if isRequiredFileType(entry)
    }
    yield createFileEntry(entry)

  protected def file(inPackage: String, name: String): Option[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage)
      entry <- Option(dirEntry.lookupName(name, directory = false))
      // no "if isRequiredFileType(entry)" check, we know exactly what we want
    }
    yield createFileEntry(entry)

  override def hasPackage(pkg: String) = findDirEntry(pkg).isDefined

  private def findDirEntry(pkg: String): Option[archive.DirEntry] =
    archive.allDirs.get(PackageNameUtils.dirPathTrailingSlashJar(pkg))

  protected def createFileEntry(file: FileZipArchive#Entry): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
}
