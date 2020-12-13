/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.io.AbstractFile
import dotty.tools.io.ClassRepresentation

case class ClassPathEntries(packages: scala.collection.Seq[PackageEntry], classesAndSources: scala.collection.Seq[ClassRepresentation]) {
  def toTuple: (scala.collection.Seq[PackageEntry], scala.collection.Seq[ClassRepresentation]) = (packages, classesAndSources)
}

object ClassPathEntries {
  val empty = ClassPathEntries(Seq.empty, Seq.empty)
}

trait ClassFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

trait SourceFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

case class PackageName(dottedString: String) {
  val dirPathTrailingSlashJar: String = FileUtils.dirPathInJar(dottedString) + "/"

  val dirPathTrailingSlash: String =
    if (java.io.File.separatorChar == '/')
      dirPathTrailingSlashJar
    else
      FileUtils.dirPath(dottedString) + java.io.File.separator

  def isRoot: Boolean = dottedString.isEmpty

  def entryName(entry: String): String = {
    if (isRoot) entry else {
      val builder = new java.lang.StringBuilder(dottedString.length + 1 + entry.length)
      builder.append(dottedString)
      builder.append('.')
      builder.append(entry)
      builder.toString
    }
  }
}

trait PackageEntry {
  def name: String
}

private[dotty] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry {
  final def fileName: String = file.name
  def name: String = FileUtils.stripClassExtension(file.name) // class name

  def binary: Option[AbstractFile] = Some(file)
  def source: Option[AbstractFile] = None
}

private[dotty] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  final def fileName: String = file.name
  def name: String = FileUtils.stripSourceExtension(file.name)

  def binary: Option[AbstractFile] = None
  def source: Option[AbstractFile] = Some(file)
}

private[dotty] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepresentation {
  final def fileName: String = classFile.name
  def name: String = FileUtils.stripClassExtension(classFile.name)

  def binary: Option[AbstractFile] = Some(classFile)
  def source: Option[AbstractFile] = Some(srcFile)
}

private[dotty] case class PackageEntryImpl(name: String) extends PackageEntry

private[dotty] trait NoSourcePaths {
  def asSourcePathString: String = ""
  private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] = Seq.empty
}

private[dotty] trait NoClassPaths {
  def findClassFile(className: String): Option[AbstractFile] = None
  private[dotty] def classes(inPackage: PackageName): Seq[ClassFileEntry] = Seq.empty
}
