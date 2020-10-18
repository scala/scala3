package dotty.tools
package dotc.fromtasty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

/** A marker CompilationUnit to return up the call stack from ReadTasty.  This will tell us that we've
 *  encountered, and attempted to inspect, a Scala2 class file (which has no .tasty file).
 *  In this case we still want to return the fact we found it so that higher-up we can take appropriate
 *  action if desired.
 */
class Scala2CompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"Scala2 class file $className"
}