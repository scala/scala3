package dotty.tools.dotc.core

import dotty.tools.io.AbstractFile
import dotty.tools.tasty.TastyVersion

/** Information about the compilation unit of a class symbol. */
trait CompilationUnitInfo:
  /** The source or class file from which this class or the class containing
   *  this symbol was generated, null if not applicable. */
  def associatedFile: AbstractFile

  /** Information about the TASTy from which this class was loaded.
   *  [[None]] if not loaded from TASTy. */
  def tastyInfo: Option[TastyInfo]

private case class ConcreteCompilationUnitInfo(
  associatedFile: AbstractFile,
  tastyInfo: Option[TastyInfo]
) extends CompilationUnitInfo

object CompilationUnitInfo:
  def apply(assocFile: AbstractFile | Null): CompilationUnitInfo | Null =
    if assocFile == null then null
    else ConcreteCompilationUnitInfo(assocFile, tastyInfo = None)

  def apply(assocFile: AbstractFile, tastyInfo: Option[TastyInfo]): CompilationUnitInfo =
    ConcreteCompilationUnitInfo(assocFile, tastyInfo)
