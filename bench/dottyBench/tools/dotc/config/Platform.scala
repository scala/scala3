package dottyBench.tools
package dotc
package config

import io.{ClassPath, AbstractFile}
import core.Contexts._, core.Symbols._
import core.SymbolLoader
import core.SymDenotations.SymDenotation
import core.StdNames.nme
import core.Flags.Module

/** The platform dependent pieces of Global.
 */
abstract class Platform {

  /** The root symbol loader. */
  def rootLoader(root: TermSymbol)(using Ctx, CState): SymbolLoader

  /** The compiler classpath. */
  def classPath(using Ctx, CState): ClassPath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit

  /** Any platform-specific phases. */
  //def platformPhases: List[SubComponent]

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isSam(cls: ClassSymbol)(using Ctx, CState): Boolean

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: ClassSymbol)(using Ctx, CState): Boolean

  /** Create a new class loader to load class file `bin` */
  def newClassLoader(bin: AbstractFile)(using Ctx, CState): SymbolLoader

  /** The given symbol is a method with the right name and signature to be a runnable program. */
  def isMainMethod(sym: SymDenotation)(using Ctx, CState): Boolean

  /** The given class has a main method. */
  final def hasMainMethod(sym: Symbol)(using Ctx, CState): Boolean =
    sym.info.member(nme.main).hasAltWith {
      case x: SymDenotation => isMainMethod(x) && (sym.is(Module) || x.isStatic)
      case _ => false
    }
}

