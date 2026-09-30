package dotty.tools.dotc.core

import dotty.tools.io.{AbstractFile, AbstractFileClassLoader}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.classpath.ClassPath

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
    val dirs: List[AbstractFile] =
      def settingsUrls: List[AbstractFile] =
        val cp = ctx.settings.classpath.value
        val entries = ClassPath.expandPath(cp, expandStar=true)
        entries.map(cp =>
          dotty.tools.io.PlainFile(dotty.tools.io.Path(cp)) // may not exist, that's OK
        )

      if ctx.mode.is(Mode.Interactive) then
        try
          ctx.platform.classPath.searchDirectories.toList
        catch
          case _: IllegalStateException =>
            settingsUrls
      else
        settingsUrls
    val out = ctx.settings.outputDir.value // to find classes in case of suspended compilation
    new AbstractFileClassLoader(dirs ++ Seq(out), getClass.getClassLoader)
  }
}
