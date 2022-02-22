package dotty.tools.scripting

import java.nio.file.{ Files, Paths, Path }
import java.io.File

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts, Contexts.{ Context, ctx }
import dotty.tools.io.{ PlainDirectory, Directory, ClassPath }
import Util.*

class ScriptingDriver(compilerArgs: Array[String], scriptFile: File, scriptArgs: Array[String]) extends Driver:
  def compileAndRun(pack:(Path, Seq[Path], String) => Boolean = null): Unit =
    val outDir = Files.createTempDirectory("scala3-scripting")
    outDir.toFile.deleteOnExit()
    setup(compilerArgs :+ scriptFile.getAbsolutePath, initCtx.fresh) match
      case Some((toCompile, rootCtx)) =>
        given Context = rootCtx.fresh.setSetting(rootCtx.settings.outputDir,
          new PlainDirectory(Directory(outDir)))

        if doCompile(newCompiler, toCompile).hasErrors then
          throw ScriptingException("Errors encountered during compilation")

        try
          val classpath = s"${ctx.settings.classpath.value}${pathsep}${sys.props("java.class.path")}"
          val classpathEntries: Seq[Path] = ClassPath.expandPath(classpath, expandStar=true).map { Paths.get(_) }
          val (mainClass, mainMethod) = detectMainClassAndMethod(outDir, classpathEntries, scriptFile.toString)
          val invokeMain: Boolean =
            Option(pack) match
              case Some(func) =>
                func(outDir, classpathEntries, mainClass)
              case None =>
                true
            end match
          if invokeMain then mainMethod.invoke(null, scriptArgs)
        catch
          case e: java.lang.reflect.InvocationTargetException =>
            throw e.getCause
        finally
          deleteFile(outDir.toFile)
      case None =>
  end compileAndRun

end ScriptingDriver

case class ScriptingException(msg: String) extends RuntimeException(msg)
