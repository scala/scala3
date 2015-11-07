package dotty.tools
package dotc
package config

import io.{AbstractFile,ClassPath,JavaClassPath,MergedClassPath,DeltaClassPath}
import ClassPath.{ JavaContext, DefaultJavaContext }
import core._
import Symbols._, Types._, Contexts._, Denotations._, SymDenotations._, StdNames._, Names._
import Flags._, Scopes._, Decorators._, NameOps._, util.Positions._

class JavaPlatform extends Platform {

  private var currentClassPath: Option[MergedClassPath] = None

  def classPath(implicit ctx: Context): ClassPath = {
    if (currentClassPath.isEmpty)
      currentClassPath = Some(new PathResolver().result)
    currentClassPath.get
  }

  // The given symbol is a method with the right name and signature to be a runnable java program.
  def isJavaMainMethod(sym: SymDenotation)(implicit ctx: Context) =
    (sym.name == nme.main) && (sym.info match {
      case t@MethodType(_, defn.ArrayOf(el) :: Nil) => el =:= defn.StringType && (t.resultType isRef defn.UnitClass)
      case _ => false
    })

  // The given class has a main method.
  def hasJavaMainMethod(sym: Symbol)(implicit ctx: Context): Boolean =
    (sym.info member nme.main).hasAltWith {
      case x: SymDenotation => isJavaMainMethod(x)
      case _ => false
    }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[ClassPath, ClassPath]) =
    currentClassPath = Some(new DeltaClassPath(currentClassPath.get, subst))

  def rootLoader(root: TermSymbol)(implicit ctx: Context): SymbolLoader = new ctx.base.loaders.PackageLoader(root, classPath)

  /** We could get away with excluding BoxedBooleanClass for the
   *  purpose of equality testing since it need not compare equal
   *  to anything but other booleans, but it should be present in
   *  case this is put to other uses.
   */
  def isMaybeBoxed(sym: ClassSymbol)(implicit ctx: Context) = {
    val d = defn
    import d._
    (sym == ObjectClass) ||
    (sym == JavaSerializableClass) ||
    (sym == ComparableClass) ||
    (sym derivesFrom BoxedNumberClass) ||
    (sym derivesFrom BoxedCharClass) ||
    (sym derivesFrom BoxedBooleanClass)
  }

  def newClassLoader(bin: AbstractFile)(implicit ctx: Context): SymbolLoader =
    new ClassfileLoader(bin)
}
