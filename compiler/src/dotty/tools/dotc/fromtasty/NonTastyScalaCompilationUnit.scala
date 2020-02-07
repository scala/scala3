package dotty.tools.dotc.fromtasty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

/**
 * A marker CompilationUnit to return up the call stack from ReadTasty.  This will tell us that we've
 * encountered, and attempted to inspect, a non-Tasty Scala class file (for example a legacy class pre-Scala 3).  
 * In this case we still want to return the fact we found it so that higher-up we can take appropriate 
 * action if desired.
 */
class NonTastyScalaCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"Non-Tasty Scala class file $className"
}