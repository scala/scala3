/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package dotty.tools
package dotc
package config

import io.{ClassPath, AbstractFile}
import core.Contexts._, core.Symbols._
import core.SymbolLoader

/** The platform dependent pieces of Global.
 */
abstract class Platform {

  /** The root symbol loader. */
  def rootLoader(root: TermSymbol)(implicit ctx: Context): SymbolLoader

  /** The compiler classpath. */
  def classPath(implicit ctx: Context): ClassPath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit

  /** Any platform-specific phases. */
  //def platformPhases: List[SubComponent]

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isSam(cls: ClassSymbol)(implicit ctx: Context): Boolean

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: ClassSymbol)(implicit ctx: Context): Boolean

  /** Create a new class loader to load class file `bin` */
  def newClassLoader(bin: AbstractFile)(implicit ctx: Context): SymbolLoader
}

