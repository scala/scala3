package dotty.tools.dotc.core

import dotty.tools.io.AbstractFile
import dotty.tools.tasty.TastyVersion

/** Information about the compilation unit of a class symbol.
  *
  * @param associatedFile The source or class file from which this class or
  *                       the class containing this symbol was generated,
  *                       null if not applicable.
  * @param tastyVersion   The TASTy version (major, minor, experimental)
  * @param tastyExplicitNulls  Was this compilation unit compiled with explicit nulls?
  */
class CompilationUnitInfo(
  val associatedFile: AbstractFile,
  val tastyVersion: Option[TastyVersion],
  val tastyExplicitNulls: Boolean
) {

  override def toString(): String =
    s"CompilationUnitInfo($associatedFile, $tastyVersion)"
}

object CompilationUnitInfo:
  def apply(assocFile: AbstractFile | Null): CompilationUnitInfo | Null =
    if assocFile == null then null
    else new CompilationUnitInfo(
      assocFile,
      tastyVersion = None,
      tastyExplicitNulls = false // TODO track explicit nulls for current compilation units (not only TASTy)
    )
