package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.reporting.trace

import scala.collection.mutable

object MacroClassLoader {

  /** A key to be used in a context property that caches the class loader used for macro expansion */
  private val MacroClassLoaderKey = new Property.Key[MacroClassLoader]

  /** Get the macro class loader */
  def fromContext(implicit ctx: Context): ClassLoader =
    ctx.property(MacroClassLoaderKey).getOrElse(makeMacroClassLoader)

  /** Context with a cached macro class loader that can be accessed with `macroClassLoader` */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(MacroClassLoaderKey, makeMacroClassLoader(ctx))

  def loadedClasses(implicit ctx: Context): List[String] =
    ctx.property(MacroClassLoaderKey).get.getLoadedClasses

  private def makeMacroClassLoader(implicit ctx: Context): MacroClassLoader = trace("new macro class loader") {
    val urls = ctx.settings.classpath.value.split(java.io.File.pathSeparatorChar).map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
    val out = ctx.settings.outputDir.value.jpath.toUri.toURL // to find classes in case of suspended compilation
    new MacroClassLoader(urls :+ out, getClass.getClassLoader)
  }
}

private class MacroClassLoader(urls: Array[java.net.URL], parent: ClassLoader) extends java.net.URLClassLoader(urls, parent) {

  private[this] val loadedClasses: mutable.SortedSet[String] = mutable.SortedSet.empty[String]

  def getLoadedClasses: List[String] = loadedClasses.toList

  override def loadClass(name: String): Class[?] = {
    loadedClasses.add(name)
    super.loadClass(name)
  }
}
