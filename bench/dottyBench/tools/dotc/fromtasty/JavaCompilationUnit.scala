package dottyBench.tools.dotc.fromtasty

import dottyBench.tools.dotc.CompilationUnit
import dottyBench.tools.dotc.util.NoSource

/** A marker CompilationUnit to return up the call stack from ReadTasty.  This will tell us that we've
 *  encountered, and attempted to inspect, a Java class file.  We can't TASTy-inspect a Java class obviously,
 *  but we want to return the fact we found it so that higher-up we can take appropriate action if desired.
 */
class JavaCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"Java class file $className"
}