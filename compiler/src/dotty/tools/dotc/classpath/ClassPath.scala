/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.io.AbstractFile
import dotty.tools.io.ClassRepresentation

case class ClassPathEntries(packages: Seq[PackageEntry], classesAndSources: Seq[ClassRepresentation]) {
  def toTuple = (packages, classesAndSources)
}

trait ClassFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

trait SourceFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

trait PackageEntry {
  def name: String
}

private[dotty] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry {
  override def name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

private[dotty] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  override def name = FileUtils.stripSourceExtension(file.name)

  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

private[dotty] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepresentation {
  override def name = FileUtils.stripClassExtension(classFile.name)

  override def binary: Option[AbstractFile] = Some(classFile)
  override def source: Option[AbstractFile] = Some(srcFile)
}

private[dotty] case class PackageEntryImpl(name: String) extends PackageEntry

private[dotty] trait NoSourcePaths {
  def asSourcePathString: String = ""
  private[dotty] def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty
}

private[dotty] trait NoClassPaths {
  def findClassFile(className: String): Option[AbstractFile] = None
  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = Seq.empty
}
