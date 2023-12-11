package dotty.tools.dotc.core

import dotty.tools.io.AbstractFile
import dotty.tools.tasty.TastyVersion

/** Information about the compilation unit of a class symbol.
  *
  * @param associatedFile The source or class file from which this class or
  *                       the class containing this symbol was generated,
  *                       null if not applicable.
  * @param tastyInfo      Information about the TASTy from which this class was loaded.
  *                       None if not loaded from TASTy,
  */
case class CompilationUnitInfo(
  associatedFile: AbstractFile,
  tastyInfo: Option[TastyInfo],
)

object CompilationUnitInfo:
  def apply(assocFile: AbstractFile | Null): CompilationUnitInfo | Null =
    if assocFile == null then null
    else new CompilationUnitInfo(assocFile, tastyInfo = None)
