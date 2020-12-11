package dotty.tools.scripting

import java.nio.file.{ Files, Path }
import java.io.File
import java.net.{ URL, URLClassLoader }
import java.lang.reflect.{ Modifier, Method }

import scala.jdk.CollectionConverters._

import dotty.tools.dotc.{ Driver, Compiler }
import dotty.tools.dotc.core.Contexts, Contexts.{ Context, ContextBase, ctx }
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.io.{ PlainDirectory, Directory }
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.config.Settings.Setting._

import sys.process._

class ScriptingDriver(compilerArgs: Array[String], scriptFile: File, scriptArgs: Array[String]) extends Driver:
  def compileAndRun(): Unit =
    val outDir = Files.createTempDirectory("scala3-scripting")
    val (toCompile, rootCtx) = setup(compilerArgs :+ scriptFile.getAbsolutePath, initCtx.fresh)
    given Context = rootCtx.fresh.setSetting(rootCtx.settings.outputDir,
      new PlainDirectory(Directory(outDir)))

    if doCompile(newCompiler, toCompile).hasErrors then
      throw ScriptingException("Errors encountered during compilation")

    try detectMainMethod(outDir, ctx.settings.classpath.value).invoke(null, scriptArgs)
    catch
      case e: java.lang.reflect.InvocationTargetException =>
        throw e.getCause
    finally
      deleteFile(outDir.toFile)
  end compileAndRun

  private def deleteFile(target: File): Unit =
    if target.isDirectory then
      for member <- target.listFiles.toList
      do deleteFile(member)
    target.delete()
  end deleteFile

  private def detectMainMethod(outDir: Path, classpath: String): Method =
    val outDirURL = outDir.toUri.toURL
    val classpathUrls = classpath.split(":").map(File(_).toURI.toURL)
    val cl = URLClassLoader(classpathUrls :+ outDirURL)

    def collectMainMethods(target: File, path: String): List[Method] =
      val nameWithoutExtension = target.getName.takeWhile(_ != '.')
      val targetPath =
        if path.nonEmpty then s"${path}.${nameWithoutExtension}"
        else nameWithoutExtension

      if target.isDirectory then
        for
          packageMember <- target.listFiles.toList
          membersMainMethod <- collectMainMethods(packageMember, targetPath)
        yield membersMainMethod
      else if target.getName.endsWith(".class") then
        val cls = cl.loadClass(targetPath)
        val method =
          try cls.getMethod("main", classOf[Array[String]])
          catch
            case _: java.lang.NoSuchMethodException => null

        if method != null && Modifier.isStatic(method.getModifiers) then List(method)
        else Nil
      else Nil
    end collectMainMethods

    val candidates = for
      file <- outDir.toFile.listFiles.toList
      method <- collectMainMethods(file, "")
    yield method

    candidates match
      case Nil =>
        throw ScriptingException("No main methods detected in your script")
      case _ :: _ :: _ =>
        throw ScriptingException("A script must contain only one main method. " +
          s"Detected the following main methods:\n${candidates.mkString("\n")}")
      case m :: Nil => m
    end match
  end detectMainMethod
end ScriptingDriver

case class ScriptingException(msg: String) extends RuntimeException(msg)
