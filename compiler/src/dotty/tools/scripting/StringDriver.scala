package dotty.tools.scripting

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts, Contexts.{ Context, ctx }
import dotty.tools.nio.*
import dotty.tools.dotc.classpath.ClassPath
import Util.*
import dotty.tools.dotc.util.SourceFile

class StringDriver(compilerArgs: Array[String], scalaSource: String) extends Driver:
  override def sourcesRequired: Boolean = false

  def compileAndRun(classpath: List[String] = Nil): Option[Throwable] =
    val outDir = FileContainer.createTemporaryOnDisk("scala3-expression")

    setup(compilerArgs, initCtx.fresh) match
      case Some((toCompile, rootCtx)) =>
        given Context = rootCtx.fresh.setSetting(rootCtx.settings.outputDir, outDir)

        val compiler = newCompiler

        val source = SourceFile.virtual("expression", scalaSource)
        compiler.newRun.compileSources(List(source))

        val output = ctx.settings.outputDir.value
        if ctx.reporter.hasErrors then
          Some(StringDriverException("Errors encountered during compilation"))
        else
          try
            val classpath = s"${ctx.settings.classpath.value}${pathsep}${sys.props("java.class.path")}"
            val classpathEntries = ClassPath.expandPath(classpath).map(FileContainer.getOrCreateOnDisk)
            sys.props("java.class.path") = classpathEntries.map(_.path).mkString(pathsep)
            detectMainClassAndMethod(outDir, classpathEntries, scalaSource) match
              case Right((mainClass, mainMethod)) =>
                mainMethod.invoke(null, Array.empty[String])
                None
              case Left(ex) => Some(ex)
          catch
            case e: java.lang.reflect.InvocationTargetException =>
              Some(e.getCause)
          finally
            outDir.deleteRecursively()
      case None => None
  end compileAndRun

end StringDriver

case class StringDriverException(msg: String) extends RuntimeException(msg)
