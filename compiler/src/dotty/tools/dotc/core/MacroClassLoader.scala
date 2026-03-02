package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.reporting.trace
import dotty.tools.io.ClassPath

object MacroClassLoader {

  /** A key to be used in a context property that caches the class loader used for macro expansion */
  private val MacroClassLoaderKey = new Property.Key[ClassLoader]

  /** Get the macro class loader */
  def fromContext(using Context): ClassLoader =
    if ctx.mode.is(Mode.Interactive) then
      // In interactive mode (:dep/:jar), classpath can change during the session.
      // Recompute on demand from the current platform classpath.
      makeMacroClassLoader
    else
      ctx.property(MacroClassLoaderKey).getOrElse(makeMacroClassLoader)

  /** Context with a cached macro class loader that can be accessed with `macroClassLoader` */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(MacroClassLoaderKey, makeMacroClassLoader(using ctx))

  private def makeMacroClassLoader(using Context): ClassLoader = trace("new macro class loader") {
    val urls: List[java.net.URL] =
      def settingsUrls: List[java.net.URL] =
        val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar=true)
        entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL).toList

      if ctx.mode.is(Mode.Interactive) then
        try
          ctx.platform.classPath.asURLs.toList
        catch
          case _: IllegalStateException =>
            settingsUrls
      else
        settingsUrls
    val out = Option(ctx.settings.outputDir.value.toURL) // to find classes in case of suspended compilation
    new java.net.URLClassLoader((urls ++ out.toList).toArray, getClass.getClassLoader)
  }
}
