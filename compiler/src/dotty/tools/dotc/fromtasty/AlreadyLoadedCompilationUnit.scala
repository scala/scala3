package dotty.tools.dotc.fromtasty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

/** A marker CompilationUnit to return up the call stack from ReadTasty.  This will tell us that we've
 *  encountered, and attempted to inspect, something that has already been loaded, for example a Scala primitive or a
 *  library class like Option.
 */
class AlreadyLoadedCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString(): String = s"AlreadyLoadedCompilationUnit(${className})"
}

