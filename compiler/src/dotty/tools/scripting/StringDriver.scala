package dotty.tools.scripting

import scala.language.unsafeNulls

import java.nio.file.{ Files, Paths, Path }

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts, Contexts.{ Context, ctx }
import dotty.tools.io.{ PlainDirectory, Directory, ClassPath }
import Util.*

class StringDriver(compilerArgs: Array[String], scalaSource: String) extends Driver:
  override def sourcesRequired: Boolean = false

  def compileAndRun(classpath: List[String] = Nil): Option[Throwable] =
    val outDir = Files.createTempDirectory("scala3-expression")
    outDir.toFile.deleteOnExit()

    setup(compilerArgs, initCtx.fresh) match
      case Some((toCompile, rootCtx)) =>
        given Context = rootCtx.fresh.setSetting(rootCtx.settings.outputDir,
          new PlainDirectory(Directory(outDir)))

        val compiler = newCompiler
        compiler.newRun.compileFromStrings(List(scalaSource))

        val output = ctx.settings.outputDir.value
        if ctx.reporter.hasErrors then
          Some(StringDriverException("Errors encountered during compilation"))

        try
          val classpath = s"${ctx.settings.classpath.value}${pathsep}${sys.props("java.class.path")}"
          val classpathEntries: Seq[Path] = ClassPath.expandPath(classpath, expandStar=true).map { Paths.get(_) }
          sys.props("java.class.path") = classpathEntries.map(_.toString).mkString(pathsep)
          val (mainClass, mainMethod) = detectMainClassAndMethod(outDir, classpathEntries, scalaSource)
          mainMethod.invoke(null, Array.empty[String])
          None
        catch
          case e: java.lang.reflect.InvocationTargetException =>
            throw e.getCause
        finally
          deleteFile(outDir.toFile)
      case None => None
  end compileAndRun

end StringDriver

case class StringDriverException(msg: String) extends RuntimeException(msg)
