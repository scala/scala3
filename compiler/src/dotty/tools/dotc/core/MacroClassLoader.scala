package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.reporting.trace
import dotty.tools.io.ClassPath

object MacroClassLoader {

  /** A key to be used in a context property that caches the class loader used for macro expansion */
  private val MacroClassLoaderKey = new Property.Key[ClassLoader]

  /** Get the macro class loader */
  def fromContext(using Context): ClassLoader =
    ctx.property(MacroClassLoaderKey).getOrElse(makeMacroClassLoader)

  /** Context with a cached macro class loader that can be accessed with `macroClassLoader` */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(MacroClassLoaderKey, makeMacroClassLoader(using ctx))

  private def makeMacroClassLoader(using Context): ClassLoader = trace("new macro class loader") {
    import scala.language.unsafeNulls

    val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar=true)
    val urls = entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL).toArray
    val out = Option(ctx.settings.outputDir.value.toURL) // to find classes in case of suspended compilation
    new java.net.URLClassLoader(urls ++ out.toList, getClass.getClassLoader)
  }
}
