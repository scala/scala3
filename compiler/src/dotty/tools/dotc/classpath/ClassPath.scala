/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.io.AbstractFile
import dotty.tools.io.ClassRepresentation

import dotty.uoption._

case class ClassPathEntries(packages: Seq[PackageEntry], classesAndSources: Seq[ClassRepresentation])

object ClassPathEntries {
  import scala.language.implicitConversions
  // to have working unzip method
  implicit def entry2Tuple(entry: ClassPathEntries): (Seq[PackageEntry], Seq[ClassRepresentation]) = (entry.packages, entry.classesAndSources)
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

  override def binary: UOption[AbstractFile] = USome(file)
  override def source: UOption[AbstractFile] = UNone
}

private[dotty] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  override def name = FileUtils.stripSourceExtension(file.name)

  override def binary: UOption[AbstractFile] = UNone
  override def source: UOption[AbstractFile] = USome(file)
}

private[dotty] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepresentation {
  override def name = FileUtils.stripClassExtension(classFile.name)

  override def binary: UOption[AbstractFile] = USome(classFile)
  override def source: UOption[AbstractFile] = USome(srcFile)
}

private[dotty] case class PackageEntryImpl(name: String) extends PackageEntry

private[dotty] trait NoSourcePaths {
  def asSourcePathString: String = ""
  private[dotty] def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty
}

private[dotty] trait NoClassPaths {
  def findClassFile(className: String): UOption[AbstractFile] = UNone
  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = Seq.empty
}
