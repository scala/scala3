package dotty.tools.dotc.core

import dotty.tools.io.AbstractFile

/** Information about the compilation unit of a class symbol. */
case class CompilationUnitInfo(
  /** The source or class file from which this class or the class containing
   *  this symbol was generated. */
  associatedFile: AbstractFile,

  /** Information about the TASTy from which this class was loaded.
   *  [[None]] if not loaded from TASTy. */
  tastyInfo: Option[TastyInfo] = None
)
