/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package dotty.tools
package dotc
package config

import io.{ClassPath, AbstractFile}
import core.Contexts._
import core.SymDenotations.ClassCompleter
import core.SymbolLoader

/** The platform dependent pieces of Global.
 */
abstract class Platform(base: ContextBase) {

  /** The root symbol loader. */
  def rootLoader: ClassCompleter

  /** The compiler classpath. */
  def classPath: ClassPath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath, ClassPath])

  /** Any platform-specific phases. */
  //def platformPhases: List[SubComponent]

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: Symbol): Boolean

  /** Create a new class loader to load class file `bin` */
  def newClassLoader(bin: AbstractFile): SymbolLoader

}

