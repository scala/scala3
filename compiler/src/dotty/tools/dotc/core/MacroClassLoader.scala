package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.reporting.trace

import scala.collection.mutable

object MacroClassLoader {

  /** A key to be used in a context property that caches the class loader used for macro expansion */
  private val MacroClassLoaderKey = new Property.Key[ClassLoader]

  /** Get the macro class loader */
  def fromContext(implicit ctx: Context): ClassLoader =
    ctx.property(MacroClassLoaderKey).getOrElse(makeMacroClassLoader)

  /** Context with a cached macro class loader that can be accessed with `macroClassLoader` */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(MacroClassLoaderKey, makeMacroClassLoader(ctx))

  private def makeMacroClassLoader(implicit ctx: Context): ClassLoader = trace("new macro class loader") {
    val urls = ctx.settings.classpath.value.split(java.io.File.pathSeparatorChar).map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
    new java.net.URLClassLoader(urls, getClass.getClassLoader)
  }
}
