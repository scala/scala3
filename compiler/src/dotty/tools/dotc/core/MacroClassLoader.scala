package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.classpath.ClassPath
import dotty.tools.nio.AbstractFileClassLoader

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
    val dirs: List[String] =
      def settingsUrls: List[String] =
        ClassPath.expandPath(ctx.settings.classpath.value)

      if ctx.mode.is(Mode.Interactive) then
        try
          ctx.platform.classPath.searchLocations.toList
        catch
          case _: IllegalStateException =>
            settingsUrls
      else
        settingsUrls
    val out = ctx.settings.outputDir.value // to find classes in case of suspended compilation
    new AbstractFileClassLoader(dirs ++ Seq(out), ctx.settings.javaOutputVersion.value, getClass.getClassLoader)
  }
}
