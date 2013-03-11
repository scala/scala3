package dotty.tools
package dotc
package config

import io.{AbstractFile,ClassPath,JavaClassPath,MergedClassPath,DeltaClassPath}
import ClassPath.{ JavaContext, DefaultJavaContext }
import core.Contexts._
import core.SymDenotations._, core.Symbols._, core.{SymbolLoader, ClassfileLoader}

class JavaPlatform extends Platform {

  private var currentClassPath: Option[MergedClassPath] = None

  def classPath(implicit ctx: Context): ClassPath = {
    if (currentClassPath.isEmpty)
      currentClassPath = Some(new PathResolver(ctx.condensed).result)
    currentClassPath.get
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[ClassPath, ClassPath]) =
    currentClassPath = Some(new DeltaClassPath(currentClassPath.get, subst))

  def rootLoader(root: TermSymbol)(implicit ctx: Context): SymbolLoader = new ctx.base.loaders.PackageLoader(root, classPath)(ctx.condensed)

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
    (sym isNonBottomSubClass BoxedNumberClass) ||
    (sym isNonBottomSubClass BoxedCharClass) ||
    (sym isNonBottomSubClass BoxedBooleanClass)
  }

  def newClassLoader(bin: AbstractFile)(implicit ctx: Context): SymbolLoader =
    new ClassfileLoader(bin)(ctx.condensed)
}
