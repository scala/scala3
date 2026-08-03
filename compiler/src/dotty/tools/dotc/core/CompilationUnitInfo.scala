package dotty.tools.dotc.core

import dotty.tools.initialize
import dotty.tools.io.AbstractFile

/** Information about the compilation unit of a class symbol. */
class CompilationUnitInfo(
  /** The source or class file from which this class or the class containing
   *  this symbol was generated. */
  val associatedFile: AbstractFile,

  // Lazy because such information requires reading files, but is not needed from many compilation units.
  tastyInfoLoader: () => Option[TastyInfo] = () => None
) {
  private var cachedTastyInfo: Option[TastyInfo] | Null = null

  /** Information about the TASTy from which this class was loaded.
   *  [[None]] if not loaded from TASTy. */
  def tastyInfo: Option[TastyInfo] =
    initialize(cachedTastyInfo, cachedTastyInfo = _, tastyInfoLoader())
}
