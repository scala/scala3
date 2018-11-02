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
import core.SymDenotations.SymDenotation
import core.StdNames.nme

/** The platform dependent pieces of Global.
 */
abstract class Platform {

  /** The root symbol loader. */
  def rootLoader(root: TermSymbol)(implicit ctx: ContextRenamed): SymbolLoader

  /** The compiler classpath. */
  def classPath(implicit ctx: ContextRenamed): ClassPath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit

  /** Any platform-specific phases. */
  //def platformPhases: List[SubComponent]

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isSam(cls: ClassSymbol)(implicit ctx: ContextRenamed): Boolean

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: ClassSymbol)(implicit ctx: ContextRenamed): Boolean

  /** Create a new class loader to load class file `bin` */
  def newClassLoader(bin: AbstractFile)(implicit ctx: ContextRenamed): SymbolLoader

  /** The given symbol is a method with the right name and signature to be a runnable program. */
  def isMainMethod(sym: SymDenotation)(implicit ctx: ContextRenamed): Boolean

  /** The given class has a main method. */
  final def hasMainMethod(sym: Symbol)(implicit ctx: ContextRenamed): Boolean =
    sym.info.member(nme.main).hasAltWith {
      case x: SymDenotation => isMainMethod(x)
      case _ => false
    }
}

