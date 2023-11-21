package dotty.tools.dotc.core

import dotty.tools.io.AbstractFile

/** Information about the compilation unit of a class symbol.
  *
  * @param associatedFile The source or class file from which this class or
  *                       the class containing this symbol was generated,
  *                       null if not applicable.
  */
class CompilationUnitInfo(
  val associatedFile: AbstractFile,
) {
  override def toString(): String =
    s"CompilationUnitInfo($associatedFile)"
}

object CompilationUnitInfo:
  def apply(assocFile: AbstractFile | Null): CompilationUnitInfo | Null =
    if assocFile == null then null
    else new CompilationUnitInfo(assocFile)
