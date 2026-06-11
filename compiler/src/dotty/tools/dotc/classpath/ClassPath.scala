/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.dotc.classpath.FileUtils.isTasty
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation, FileExtension}

trait PackageEntry {
  def name: String
}

/** A TASTy file or classfile */
sealed trait BinaryFileEntry extends ClassRepresentation {
  def file: AbstractFile
  final def fileName: String = file.name
  final def name: String = FileUtils.stripExtension(file.name) // class name
  final def source: Option[AbstractFile] = None
}

object BinaryFileEntry {
  def apply(file: AbstractFile): BinaryFileEntry =
    if file.isTasty then
      if file.resolveSiblingWithExtension(FileExtension.Class) != null then TastyWithClassFileEntry(file)
      else StandaloneTastyFileEntry(file)
    else
      ClassFileEntry(file)
}

/** A classfile or .sig that does not have an associated TASTy file */
private[dotty] final case class ClassFileEntry(file: AbstractFile) extends BinaryFileEntry {
  def binary: Option[AbstractFile] = Some(file)
}

/** A TASTy file that has an associated class file */
private[dotty] final case class TastyWithClassFileEntry(file: AbstractFile) extends BinaryFileEntry {
  def binary: Option[AbstractFile] = Some(file)
}

/** A TASTy file that does not have an associated class file */
private[dotty] final case class StandaloneTastyFileEntry(file: AbstractFile) extends BinaryFileEntry {
  def binary: Option[AbstractFile] = Some(file)
}

private[dotty] final case class SourceFileEntry(file: AbstractFile) extends ClassRepresentation {
  def fileName: String = file.name
  def name: String = FileUtils.stripSourceExtension(file.name)
  def binary: Option[AbstractFile] = None
  def source: Option[AbstractFile] = Some(file)
}

private[dotty] final case class BinaryAndSourceFilesEntry(binaryEntry: BinaryFileEntry, sourceEntry: SourceFileEntry) extends ClassRepresentation {
  def fileName: String = binaryEntry.fileName
  def name: String = binaryEntry.name
  def binary: Option[AbstractFile] = binaryEntry.binary
  def source: Option[AbstractFile] = sourceEntry.source
}

private[dotty] case class PackageEntryImpl(name: String) extends PackageEntry

private[dotty] trait NoSourcePaths {
  def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty
}

private[dotty] trait NoClassPaths {
  def findClassFile(className: String): Option[AbstractFile] = None
  def classes(inPackage: String): Seq[BinaryFileEntry] = Seq.empty
}
