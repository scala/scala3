package dottyBench.tools.dotc.core

import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.util.Property
import dottyBench.tools.dotc.reporting.trace

import scala.collection.mutable

object MacroClassLoader {

  /** A key to be used in a context property that caches the class loader used for macro expansion */
  private val MacroClassLoaderKey = new Property.Key[ClassLoader]

  /** Get the macro class loader */
  def fromContext(using Context): ClassLoader =
    ctx.property(MacroClassLoaderKey).getOrElse(makeMacroClassLoader)

  /** Context with a cached macro class loader that can be accessed with `macroClassLoader` */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(MacroClassLoaderKey, makeMacroClassLoader(using ctx))

  private def makeMacroClassLoader(using Context): ClassLoader =
    given CState = currentContext.cstate
    trace("new macro class loader") {
      val urls = ctx.settings.classpath.value.split(java.io.File.pathSeparatorChar).map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
      val out = ctx.settings.outputDir.value.jpath.toUri.toURL // to find classes in case of suspended compilation
      new java.net.URLClassLoader(urls :+ out, getClass.getClassLoader)
    }
}
